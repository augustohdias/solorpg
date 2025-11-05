{-# LANGUAGE OverloadedStrings #-}
module System.Impl.ActionService (newHandle, isPayThePriceTrigger) where

import qualified System.ActionContract as Action
import qualified System.DiceContract as Dice
import qualified System.GameContextContract as GameContext
import qualified System.MoveContract as Move
import qualified System.ProgressContract as Progress
import qualified System.OracleContract as Oracle
import qualified System.HelpContract as Help
import qualified System.Constants as C
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import System.Tui.Comm (GameOutput(..))
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad (unless, void)
import Data.Foldable (for_)

newHandle :: Dice.Handle -> GameContext.Handle -> Move.Handle -> Progress.Handle -> Oracle.Handle -> Help.Handle -> TChan GameOutput -> IO Action.Handle
newHandle diceHandler contextHandler moveHandler progressHandler oracleHandler helpHandler outputChan =
  return $
    Action.Handle
      { Action.process = processAction
      }
  where
    processAction actionType = case actionType of
      Action.AddStoryLog -> addStoryLog
      Action.RollDice -> rollDice
      Action.Show -> showLogs
      Action.Exit -> exit
      Action.CreateCharacter -> createCharacter
      Action.LoadCharacter -> loadCharacter
      Action.ShowCharacter -> showCharacter
      Action.UpdateAttribute -> updateAttribute
      Action.UpdateResource -> updateResource
      Action.AddAttribute -> addAttribute
      Action.AddResource -> addResource
      Action.Challenge -> challenge
      Action.Move -> moveAction
      Action.SwearVow -> swearVow
      Action.MarkProgress -> markProgress
      Action.RollProgress -> rollProgress
      Action.ShowTracks -> showTracks
      Action.AbandonTrack -> abandonTrack
      Action.Oracle -> oracleQuery
      Action.Help -> helpCommand
      Action.Bond -> bondCommand
      _ -> defaultAction

    defaultAction _ = return False

    -- | Helper for sending log messages to the TUI
    logMessage :: T.Text -> IO ()
    logMessage msg = atomically $ writeTChan outputChan (LogEntry msg)

    -- | Executa uma ação que requer um contexto de personagem carregado.
    -- Mostra uma mensagem de erro se nenhum personagem estiver carregado.
    withContext :: (GameContext.Context -> IO Bool) -> IO Bool
    withContext action = do
      maybeCtx <- GameContext.getCurrentContext contextHandler
      case maybeCtx of
        Nothing -> do
          logMessage (C.msgNoCharacterLoaded C.messages)
          return True
        Just ctx -> action ctx

    -- | Executa uma ação em um contexto, se ele existir, mas não faz nada
    -- (e não mostra erro) se o contexto for Nothing.
    whenContext :: (GameContext.Context -> IO ()) -> IO ()
    whenContext action = do
      maybeCtx <- GameContext.getCurrentContext contextHandler
      for_ maybeCtx action

    addStoryLog story = do
      -- Ignora entradas que começam com : (são comandos, não narrativa)
      if T.isPrefixOf ":" story
        then return True
        else do
          whenContext $ \ctx -> do
            ctxWithLog <- GameContext.addLogEntry contextHandler ctx story
            void $ GameContext.saveContext contextHandler ctxWithLog
          -- Always send story log to TUI
          logMessage story
          return True

    rollDice diceDescription = do
      rolls <- Dice.roll diceHandler diceDescription
      let msg = C.msgDiceRolled C.messages <> T.pack (show rolls)
      logMessage msg

      -- Se há contexto, adiciona ao log do personagem
      whenContext $ \ctx -> do
        ctxWithLog <- GameContext.addLogEntry contextHandler ctx msg
        void $ GameContext.saveContext contextHandler ctxWithLog
      return True

    showLogs _ = withContext $ \ctx -> do
      logMessage (C.msgLogsHeader C.messages)
      let logs = GameContext.getSessionLog contextHandler ctx
      mapM_ logMessage logs
      return True

    exit _ = do
      logMessage (C.msgSessionEnded C.messages)
      atomically $ writeTChan outputChan GameEnd
      return False

    -- Funções para gerenciamento de contexto
    createCharacter input = do
      -- Input esperado: "NomePersonagem iron:3 edge:2 heart:2 shadow:1 wits:2"
      let parts = T.words input
      if null parts
        then do
          logMessage (C.msgCharacterNameRequired C.messages)
          return True
        else do
          let charName = head parts
          let attrParts = tail parts

          -- Parse dos atributos (valores padrão se não fornecidos)
          let attrs = parseAttributes attrParts

          result <- GameContext.createContext contextHandler charName attrs
          case result of
            Left err -> do
              logMessage (C.msgErrorCreating C.messages <> T.pack (show err))
              return True
            Right ctx -> do
              let msg = C.msgCharacterCreated C.messages <> GameContext.getCharacterName ctx
              logMessage msg

              -- Não adiciona ao log (apenas mensagem de sistema)
              _ <- GameContext.saveContext contextHandler ctx
              atomically $ writeTChan outputChan (CharacterUpdate (GameContext.mainCharacter ctx))
              return True

    loadCharacter charName = do
      if T.null charName
        then do
          logMessage (C.msgCharacterNameRequired C.messages)
          return True
        else do
          result <- GameContext.loadContext contextHandler charName
          case result of
            Left err -> do
              logMessage (C.msgErrorLoading C.messages <> T.pack (show err))
              return True
            Right ctx -> do
              let msg = C.msgCharacterLoaded C.messages <> GameContext.getCharacterName ctx
              logMessage msg

              -- Não adiciona ao log (apenas mensagem de sistema)
              _ <- GameContext.saveContext contextHandler ctx
              atomically $ writeTChan outputChan (CharacterUpdate (GameContext.mainCharacter ctx))
              return True

    showCharacter _ = withContext $ \ctx -> do
      let char = GameContext.mainCharacter ctx
      atomically $ writeTChan outputChan (CharacterUpdate char)
      logMessage "Character sheet updated."
      return True

    updateAttribute input = withContext $ \ctx -> do
      let oldAttrs = GameContext.attributes (GameContext.mainCharacter ctx)
      case parseAttributeUpdate input oldAttrs of
        Nothing -> do
          logMessage (C.msgInvalidAttributeFormat C.messages)
          return True
        Just newAttrs -> do
          updatedCtx <- GameContext.updateAttributes contextHandler ctx newAttrs
          _ <- GameContext.saveContext contextHandler updatedCtx
          logMessage (C.msgAttributeUpdated C.messages)
          atomically $ writeTChan outputChan (CharacterUpdate (GameContext.mainCharacter updatedCtx))
          return True

    updateResource input = withContext $ \ctx -> do
      let oldRes = GameContext.resources (GameContext.mainCharacter ctx)
      case parseResourceUpdate input oldRes of
        Nothing -> do
          logMessage (C.msgInvalidResourceFormat C.messages)
          return True
        Just newRes -> do
          updatedCtx <- GameContext.updateResources contextHandler ctx newRes
          _ <- GameContext.saveContext contextHandler updatedCtx
          logMessage (C.msgResourceUpdated C.messages)
          atomically $ writeTChan outputChan (CharacterUpdate (GameContext.mainCharacter updatedCtx))
          return True

    addAttribute input = withContext $ \ctx -> do
      let oldAttrs = GameContext.attributes (GameContext.mainCharacter ctx)
      case parseAttributeAdd input oldAttrs of
        Nothing -> do
          logMessage "Formato inválido. Use: atributo:delta (ex: iron:-1, edge:+2)"
          return True
        Just newAttrs -> do
          updatedCtx <- GameContext.updateAttributes contextHandler ctx newAttrs
          _ <- GameContext.saveContext contextHandler updatedCtx
          logMessage (C.msgAttributeUpdated C.messages)
          atomically $ writeTChan outputChan (CharacterUpdate (GameContext.mainCharacter updatedCtx))
          return True

    addResource input = withContext $ \ctx -> do
      let oldRes = GameContext.resources (GameContext.mainCharacter ctx)
      case parseResourceAdd input oldRes of
        Nothing -> do
          logMessage "Formato inválido. Use: recurso:delta (ex: health:-1, momentum:+2)"
          return True
        Just newRes -> do
          updatedCtx <- GameContext.updateResources contextHandler ctx newRes
          _ <- GameContext.saveContext contextHandler updatedCtx
          logMessage (C.msgResourceUpdated C.messages)
          atomically $ writeTChan outputChan (CharacterUpdate (GameContext.mainCharacter updatedCtx))
          return True

    challenge _ = do
      -- Rola 1d6,2d10 automaticamente e avalia
      rolls <- Dice.roll diceHandler "1d6,2d10"
      case rolls of
        [(_, actionDie), (_, ch1), (_, ch2)] -> do
          let result = evaluateActionRoll actionDie ch1 ch2
          let resultMsg = showActionRollResult result
          let interpretation = case result of
                Action.StrongHit -> C.challengeStrongHit C.challengeInterpretation
                Action.WeakHit -> C.challengeWeakHit C.challengeInterpretation
                Action.Miss -> C.challengeMiss C.challengeInterpretation
                Action.InvalidRoll -> ""
          let matchMsg = if ch1 == ch2 then C.challengeMatch C.challengeInterpretation else ""

          let formattedMsg = T.pack $ C.formatActionRoll C.characterDisplay actionDie ch1 ch2 resultMsg
                          ++ interpretation ++ matchMsg
          logMessage formattedMsg

          return True
        _ -> do
          logMessage "Erro ao rolar dados para challenge"
          return True

    moveAction input = withContext $ \ctx -> do
      let parts = T.words input
      case parts of
        [] -> do
          logMessage $ T.pack (C.msgMoveUsage C.helpMessages)
          return True
        (moveName:params) ->
          case Move.parseMoveType moveHandler moveName of
            Nothing -> do
              logMessage $ "Move desconhecido: " <> T.unwords parts <> "\n" <> T.pack (C.msgMovesAvailable C.helpMessages)
              return True
            Just moveType -> do
              let (maybeStat, cleanParams) = parseMoveParams params
              executeMoveByCategory moveType maybeStat cleanParams ctx

    -- | Separa os parâmetros de um comando de move em stat (opcional) e o resto.
    parseMoveParams :: [T.Text] -> (Maybe Move.Stat, T.Text)
    parseMoveParams params =
      case params of
        (first:rest) ->
          case Move.parseStat moveHandler first of
            Just s  -> (Just s, T.unwords rest)
            Nothing -> (Nothing, T.unwords params)
        [] -> (Nothing, "")

    -- | Direciona a execução do move baseado na sua categoria.
    executeMoveByCategory :: Move.MoveType -> Maybe Move.Stat -> T.Text -> GameContext.Context -> IO Bool
    executeMoveByCategory moveType maybeStat cleanParams ctx =
      case moveType of
        Move.SwearIronVow         -> executeMoveSwearVow cleanParams ctx
        Move.FulfillYourVow       -> executeMoveFulfillVow cleanParams ctx
        Move.UndertakeJourney     -> executeMoveUndertakeJourney maybeStat ctx
        Move.ReachYourDestination -> executeMoveReachDestination cleanParams ctx
        Move.EndTheFight          -> executeMoveEndFight cleanParams ctx
        Move.ReachMilestone       -> executeMoveMilestone cleanParams ctx
        Move.PayThePrice          -> executePayThePrice ctx
        _                         -> executeStandardMove moveType maybeStat ctx

    -- | Executa um move padrão (com rolagem de dados e consequências).
    executeStandardMove :: Move.MoveType -> Maybe Move.Stat -> GameContext.Context -> IO Bool
    executeStandardMove moveType maybeStat ctx = do
      (moveResult, updatedCtx) <- Move.executeMove moveHandler moveType maybeStat ctx contextHandler

      -- Salva o estado atualizado após o move
      let updatedRes = GameContext.resources (GameContext.mainCharacter updatedCtx)
      ctxWithUpdatedRes <- GameContext.updateResources contextHandler updatedCtx updatedRes
      _ <- GameContext.saveContext contextHandler ctxWithUpdatedRes

      -- Verifica se há moves triggered (PayThePrice, FaceDeath, etc)
      let triggers = [m | Move.TriggerMove m <- Move.consequencesApplied moveResult]
      case listToMaybe triggers of
        Just triggeredMove -> executeTriggeredMove triggeredMove ctxWithUpdatedRes
        Nothing -> return True

    -- Funções auxiliares para progress moves via :move
    executeMoveSwearVow input ctx = do
      -- Parse: vow "Nome" rank ou apenas "Nome" rank (já parseado)
      case parseQuotedString input of
        Just (vowName, rest) -> do
          let rankText = T.strip rest
          case parseRank rankText of
            Nothing -> do
              logMessage $ T.pack (C.msgVowUsage C.moveMessages)
              return True
            Just rank -> do
              -- Cria track
              let track = Progress.newProgressTrack vowName Progress.Vow rank
              ctxWithTrack <- GameContext.addProgressTrack contextHandler ctx track

              -- Executa o move (roll +heart)
              (_, updatedCtx) <- Move.executeMove moveHandler Move.SwearIronVow (Just Move.Heart) ctxWithTrack contextHandler

              let updatedRes = GameContext.resources (GameContext.mainCharacter updatedCtx)
              ctxFinal <- GameContext.updateResources contextHandler updatedCtx updatedRes
              _ <- GameContext.saveContext contextHandler ctxFinal

              logMessage $ T.pack $ C.formatVowCreated C.moveMessages vowName (show rank) (Progress.getTicksForRank rank)
              return True
        Nothing -> do
          logMessage $ T.pack (C.msgVowUsage C.moveMessages)
          return True

    executeMoveFulfillVow input ctx = do
      let trackName = case parseQuotedString input of
            Just (name, _) -> name
            Nothing -> T.strip input

      case GameContext.getProgressTrack contextHandler ctx trackName of
        Nothing -> do
          logMessage $ T.pack (C.errVowNotFound C.errorMessages) <> trackName
          return True
        Just track -> do
          result <- Progress.rollProgress progressHandler track
          interpretVowRoll logMessage result

          -- Se strong ou weak hit, completa o track
          case Progress.progressRollResult result of
            Action.StrongHit -> void $ completeTrackAndUpdateContext track
            Action.WeakHit   -> void $ completeTrackAndUpdateContext track
            _                -> return ()

          return True
      where
        completeTrackAndUpdateContext track = do
          completed <- Progress.completeTrack progressHandler track
          updatedCtx <- GameContext.updateProgressTrack contextHandler ctx (Progress.trackName track) completed
          _ <- GameContext.saveContext contextHandler updatedCtx
          return ()

    executeMoveUndertakeJourney _maybeStat _ctx = do
      logMessage $ T.pack (C.msgUndertakeJourney C.moveMessages)
      return True

    executeMoveReachDestination input ctx = do
      let trackName = case parseQuotedString input of
            Just (name, _) -> name
            Nothing -> T.strip input

      case GameContext.getProgressTrack contextHandler ctx trackName of
        Nothing -> do
          logMessage $ C.msgTrackNotFound C.moveMessages <> trackName
          return True
        Just track -> do
          result <- Progress.rollProgress progressHandler track
          interpretJourneyRoll logMessage result
          return True

    executeMoveEndFight input ctx = do
      let trackName = case parseQuotedString input of
            Just (name, _) -> name
            Nothing -> T.strip input

      case GameContext.getProgressTrack contextHandler ctx trackName of
        Nothing -> do
          logMessage $ C.msgTrackNotFound C.moveMessages <> trackName
          return True
        Just track -> do
          result <- Progress.rollProgress progressHandler track
          interpretCombatRoll logMessage result
          return True

    executePayThePrice ctx = do
      -- Default to option 3: Roll on the Pay the Price table
      logMessage "\n>>> Pay the Price <<<"
      result <- Oracle.rollOracle oracleHandler "Pay the Price"
      case result of
        Left err -> do
          logMessage $ "Erro ao consultar oráculo: " <> T.pack (show err)
          return True
        Right oracleResult -> do
          logMessage $ "🔮 Pay the Price (Rolou " <> T.pack (show (Oracle.resultRoll oracleResult)) <> "):"
          logMessage $ "→ " <> Oracle.resultText oracleResult

          -- Executa consequência se houver
          executeOracleConsequence oracleResult

          _ <- GameContext.saveContext contextHandler ctx
          return True

    executeMoveMilestone input ctx = do
      let trackName = parseTrackNameFromInput input

      case GameContext.getProgressTrack contextHandler ctx trackName of
        Nothing -> do
          logMessage $ T.pack (C.errVowNotFound C.errorMessages) <> trackName
          return True
        Just track -> do
          updatedTrack <- Progress.markProgress progressHandler track
          updatedCtx <- GameContext.updateProgressTrack contextHandler ctx trackName updatedTrack
          _ <- GameContext.saveContext contextHandler updatedCtx
          logMessage (C.msgProgressMarked C.moveMessages)
          return True

    -- | Extrai o nome de um track do input do usuário (com ou sem aspas).
    parseTrackNameFromInput :: T.Text -> T.Text
    parseTrackNameFromInput input =
      maybe (T.strip input) fst (parseQuotedString input)

    -- Comandos de Progress Tracks
    swearVow input = withContext $ \ctx -> do
      -- Parse: :vow "Nome Do Voto" rank
      case parseQuotedString input of
        Just (vowName, rest) -> do
          let rankText = T.strip rest
          case parseRank rankText of
            Nothing -> do
              logMessage $ T.pack (C.errInvalidRank C.errorMessages) <> rankText <> "\n" <> T.pack (C.msgRanksAvailable C.helpMessages)
              return True
            Just rank -> do
              let track = Progress.newProgressTrack vowName Progress.Vow rank
              updatedCtx <- GameContext.addProgressTrack contextHandler ctx track
              _ <- GameContext.saveContext contextHandler updatedCtx

              logMessage $ T.pack $ C.formatVowCreated C.moveMessages vowName (show rank) (Progress.getTicksForRank rank)
              return True
        Nothing -> do
          logMessage $ T.pack (C.msgVowUsage C.moveMessages)
          return True

    markProgress input = withContext $ \ctx -> do
      let trackName = parseTrackNameFromInput input

      case GameContext.getProgressTrack contextHandler ctx trackName of
        Nothing -> do
          logMessage $ C.msgTrackNotFound C.moveMessages <> trackName <> "\n" <> T.pack (C.msgProgressUsage C.moveMessages)
          return True
        Just track -> do
          updatedTrack <- Progress.markProgress progressHandler track
          updatedCtx <- GameContext.updateProgressTrack contextHandler ctx trackName updatedTrack
          _ <- GameContext.saveContext contextHandler updatedCtx
          logMessage "Progress marked." -- Or some other confirmation
          return True

    rollProgress input = withContext $ \ctx -> do
      let trackName = parseTrackNameFromInput input

      case GameContext.getProgressTrack contextHandler ctx trackName of
        Nothing -> do
          logMessage $ C.msgTrackNotFound C.moveMessages <> trackName
          return True
        Just track -> do
          result <- Progress.rollProgress progressHandler track

          -- Interpreta resultado baseado no tipo
          case Progress.trackType track of
            Progress.Vow     -> interpretVowRoll logMessage result
            Progress.Combat  -> interpretCombatRoll logMessage result
            Progress.Journey -> interpretJourneyRoll logMessage result

          return True

    showTracks _ = withContext $ \ctx -> do
      let tracks = GameContext.progressTracks ctx
      if null tracks
        then logMessage $ T.pack (C.msgNoTracksActive C.moveMessages)
        else do
          logMessage $ T.pack (C.msgTracksHeader C.moveMessages)
          mapM_ showTrack tracks
      return True
      where
        showTrack track = do
          let boxes = Progress.getProgressScore track
          let ticks = Progress.trackTicks track
          let percentage = Progress.progressPercentage track
          let formattedTrack = T.pack $ C.formatProgressTrack C.moveMessages
                (Progress.trackName track)
                (show $ Progress.trackType track)
                (show $ Progress.trackRank track)
                boxes
                ticks
                percentage
                (Progress.trackCompleted track)
          logMessage formattedTrack

    abandonTrack input = withContext $ \ctx -> do
      let trackName = parseTrackNameFromInput input

      updatedCtx <- GameContext.removeProgressTrack contextHandler ctx trackName
      _ <- GameContext.saveContext contextHandler updatedCtx
      logMessage $ C.msgTrackRemoved C.moveMessages <> trackName
      return True

    oracleQuery input = do
      if T.null (T.strip input)
        then listOracles
        else queryOracle input

    listOracles = do
      oracles <- Oracle.listOracles oracleHandler
      if null oracles
        then logMessage $ C.msgNoOraclesLoaded C.moveMessages
        else do
          logMessage $ T.pack (C.msgOraclesHeader C.moveMessages)
          mapM_ (logMessage . ("• " <>)) oracles
      return True

    queryOracle input = do
      let (oracleName, maybeValue) = parseOracleQuery input
      if T.null maybeValue
        then rollOracleRandomly oracleName
        else rollOracleWithGivenValue oracleName maybeValue

    parseOracleQuery input =
      case parseQuotedString input of
        Just (name, rest) -> (name, T.strip rest)
        Nothing ->
          let parts = T.words input
          in if null parts
             then ("", "")
             else (head parts, T.unwords (tail parts))

    rollOracleRandomly oracleName = do
      result <- Oracle.rollOracle oracleHandler oracleName
      case result of
        Left err -> do
          logMessage $ T.pack (C.errOracleError C.errorMessages) <> T.pack (show err)
          return True
        Right oracleResult -> do
          let formattedResult = T.pack $ C.formatOracleRoll C.moveMessages
                                (Oracle.resultOracle oracleResult)
                                (Oracle.resultRoll oracleResult)
                                (Oracle.resultText oracleResult)
          logMessage formattedResult
          executeOracleConsequence oracleResult
          return True

    rollOracleWithGivenValue oracleName valueText = do
      case TR.decimal valueText of
        Right (val, _) -> do
          result <- Oracle.queryOracle oracleHandler oracleName val
          case result of
            Left err -> do
              logMessage $ T.pack (C.errOracleError C.errorMessages) <> T.pack (show err)
              return True
            Right oracleResult -> do
              let formattedResult = T.pack $ C.formatOracleIndex C.moveMessages
                                    (Oracle.resultOracle oracleResult)
                                    (Oracle.resultRoll oracleResult)
                                    (Oracle.resultText oracleResult)
              logMessage formattedResult
              executeOracleConsequence oracleResult
              return True
        Left _ -> do
          logMessage $ T.pack (C.errInvalidValue C.errorMessages)
          return True

    helpCommand input = do
      let topic = T.strip input
      if T.null topic
        then do
          helpText <- Help.showHelp helpHandler
          logMessage (T.pack helpText)
        else case Help.parseTopic helpHandler topic of
          Just t -> do
            helpText <- Help.showTopicHelp helpHandler t
            logMessage (T.pack helpText)
          Nothing -> do
            logMessage $ "Tópico desconhecido: " <> topic
            logMessage "Tópicos disponíveis: moves, progress, oracle, chaining, character"
      return True

    bondCommand input = withContext $ \ctx -> do
      let parts = T.words input
      case parts of
        []            -> listBonds ctx
        ("add":rest)    -> addBond (T.unwords rest) ctx
        ("remove":rest) -> removeBond (T.unwords rest) ctx
        ("check":rest)  -> checkBond (T.unwords rest) ctx
        _               -> showBondUsage

    listBonds ctx = do
      let allBonds = GameContext.listBonds contextHandler ctx
      if null allBonds
        then logMessage "\nNenhum vínculo (bond) estabelecido."
        else do
          logMessage "\n=== Vínculos (Bonds) ==="
          mapM_ showBond allBonds
      return True

    addBond input ctx = do
      case parseQuotedString input of
        Just (bondName, after) -> do
          let afterParts = T.words after
          let bondTypeText = if null afterParts then "person" else head afterParts
          let notes = T.unwords (if null afterParts then [] else tail afterParts)

          let bondType = case T.toLower bondTypeText of
                "community" -> GameContext.CommunityBond
                _           -> GameContext.PersonBond

          let bond = GameContext.Bond bondName bondType notes
          updatedCtx <- GameContext.addBond contextHandler ctx bond
          _ <- GameContext.saveContext contextHandler updatedCtx

          logMessage $ "\n✓ Vínculo criado: " <> bondName
          return True
        Nothing -> showBondUsage

    removeBond input ctx = do
      let bondName = maybe (T.unwords . T.words $ input) fst (parseQuotedString input)
      updatedCtx <- GameContext.removeBond contextHandler ctx bondName
      _ <- GameContext.saveContext contextHandler updatedCtx
      logMessage $ "Vínculo removido: " <> bondName
      return True

    checkBond input ctx = do
      let bondName = maybe (T.unwords . T.words $ input) fst (parseQuotedString input)
      if GameContext.hasBond contextHandler ctx bondName
        then logMessage $ "✓ Você tem vínculo com: " <> bondName
        else logMessage $ "✗ Você NÃO tem vínculo com: " <> bondName
      return True

    showBondUsage = do
      logMessage "Uso: :bond [add|remove|check] ou apenas :bond para listar"
      return True

    showBond bond = do
      let typeStr = case GameContext.bondType bond of
            GameContext.PersonBond    -> "Pessoa"
            GameContext.CommunityBond -> "Comunidade"
      logMessage $ "\n• " <> GameContext.bondName bond <> " (" <> T.pack typeStr <> ")"
      unless (T.null $ GameContext.bondNotes bond) $
        logMessage $ "  " <> GameContext.bondNotes bond

    -- | Executa move triggered
    executeTriggeredMove triggeredMove ctx = do
      logMessage $ "\n>>> Executando " <> Move.moveTypeToText triggeredMove <> " automaticamente..."
      (moveResult, updatedCtx) <- Move.executeMove moveHandler triggeredMove Nothing ctx contextHandler

      let updatedRes = GameContext.resources (GameContext.mainCharacter updatedCtx)
      ctxFinal <- GameContext.updateResources contextHandler updatedCtx updatedRes
      _ <- GameContext.saveContext contextHandler ctxFinal

      -- Verifica se gerou mais triggers (moves ou oráculos)
      let moveTriggers = [m | Move.TriggerMove m <- Move.consequencesApplied moveResult]
      let oracleTriggers = [o | Move.TriggerOracle o <- Move.consequencesApplied moveResult]

      case (moveTriggers, oracleTriggers) of
        (m:_, _) -> executeTriggeredMove m ctxFinal
        ([], o:_) -> executeTriggeredOracle o ctxFinal
        _ -> return True

    -- | Executa oráculo triggered (verifica condições antes)
    executeTriggeredOracle oracleName ctx = do
      let char = GameContext.mainCharacter ctx
      let res = GameContext.resources char

      -- Verifica se deve executar o oráculo
      let shouldExecute = case T.unpack oracleName of
            "Endure Harm"   -> GameContext.health res == 0
            "Endure Stress" -> GameContext.spirit res == 0
            _               -> True  -- Outros oráculos sempre executam

      if not shouldExecute
        then do
          -- Condição não atendida, apenas continua
          _ <- GameContext.saveContext contextHandler ctx
          return True
        else do
          logMessage $ "\n🔮 Consultando oráculo " <> oracleName <> " automaticamente..."
          result <- Oracle.rollOracle oracleHandler oracleName
          case result of
            Left err -> do
              logMessage $ "Erro: " <> T.pack (show err)
              _ <- GameContext.saveContext contextHandler ctx
              return True
            Right oracleResult -> do
              logMessage $ "→ " <> Oracle.resultText oracleResult

              -- Executa consequência automaticamente
              executeOracleConsequence oracleResult

              _ <- GameContext.saveContext contextHandler ctx
              return True

    -- | Executa consequência de resultado de oráculo AUTOMATICAMENTE
    executeOracleConsequence oracleResult =
      for_ (Oracle.resultConsequence oracleResult) $ \consText -> do
        maybeCtx <- GameContext.getCurrentContext contextHandler
        for_ maybeCtx $ \ctx -> do
          let parts = T.splitOn ":" consText
          case parts of
            ("move":moveNameParts) -> do
              let moveName = T.unwords moveNameParts
              logMessage $ "\n  ⚡ Executando: :move " <> moveName
              -- Executa o move automaticamente
              _ <- Move.executeMove moveHandler
                (fromMaybe Move.PayThePrice (Move.parseMoveType moveHandler moveName))
                Nothing
                ctx
                contextHandler
              return ()

            ("resource":resName:valueParts) -> do
              let valueText = T.unwords valueParts
              -- Executa :addres automaticamente
              let command = resName <> ":" <> valueText
              let char = GameContext.mainCharacter ctx
              let oldRes = GameContext.resources char

              for_ (parseResourceAdd command oldRes) $ \newRes -> do
                updatedCtx <- GameContext.updateResources contextHandler ctx newRes
                _ <- GameContext.saveContext contextHandler updatedCtx
                logMessage $ "\n  ⚡ " <> resName <> ": " <> valueText <> " (aplicado)"

            _ -> logMessage $ "\n  ⚡ Consequência: " <> consText

-- Parse atributos de uma lista de textos (ex: ["iron:3", "edge:2"])
parseAttributes :: [T.Text] -> GameContext.Attributes
parseAttributes parts =
  let defVal = C.configDefaultAttributeValue C.defaultConfig
      defaultAttrs = GameContext.Attributes defVal defVal defVal defVal defVal
      updateAttr attr key val =
        case key of
          "iron" -> attr { GameContext.iron = val }
          "edge" -> attr { GameContext.edge = val }
          "heart" -> attr { GameContext.heart = val }
          "shadow" -> attr { GameContext.shadow = val }
          "wits" -> attr { GameContext.wits = val }
          _ -> attr
  in foldl (\acc part ->
        case T.splitOn ":" part of
          [k, v] -> case TR.decimal v of
            Right (n, _) -> updateAttr acc k n
            _ -> acc
          _ -> acc
      ) defaultAttrs parts

-- | Parse atualização de atributo
parseAttributeUpdate :: T.Text -> GameContext.Attributes -> Maybe GameContext.Attributes
parseAttributeUpdate input attrs = do
  (key, valStr) <- parseKeyValue input
  n <- parseDecimal valStr
  case key of
    "iron"   -> Just $ attrs { GameContext.iron = n }
    "edge"   -> Just $ attrs { GameContext.edge = n }
    "heart"  -> Just $ attrs { GameContext.heart = n }
    "shadow" -> Just $ attrs { GameContext.shadow = n }
    "wits"   -> Just $ attrs { GameContext.wits = n }
    _        -> Nothing

-- | Parse definição de recurso (apenas define valor absoluto)
parseResourceUpdate :: T.Text -> GameContext.Resources -> Maybe GameContext.Resources
parseResourceUpdate input res = do
  (key, valStr) <- parseKeyValue input
  n <- parseDecimal valStr
  case key of
    "spirit"     -> Just $ res { GameContext.spirit = n }
    "health"     -> Just $ res { GameContext.health = n }
    "supply"     -> Just $ res { GameContext.supply = n }
    "momentum"   -> Just $ res { GameContext.momentum = n }
    "experience" -> Just $ res { GameContext.experience = n }
    _            -> Nothing

-- | Parse adição/remoção de recurso (delta com valores negativos)
parseResourceAdd :: T.Text -> GameContext.Resources -> Maybe GameContext.Resources
parseResourceAdd input res = do
  (key, valStr) <- parseKeyValue input
  delta <- parseSignedDecimal valStr
  case key of
    "spirit"     -> Just $ res { GameContext.spirit     = clamp 0 5 (GameContext.spirit res + delta) }
    "health"     -> Just $ res { GameContext.health     = clamp 0 5 (GameContext.health res + delta) }
    "supply"     -> Just $ res { GameContext.supply     = clamp 0 5 (GameContext.supply res + delta) }
    "momentum"   -> Just $ res { GameContext.momentum   = GameContext.momentum res + delta }
    "experience" -> Just $ res { GameContext.experience = max 0 (GameContext.experience res + delta) }
    _            -> Nothing

-- | Parse adição/remoção de atributo (delta com valores negativos)
parseAttributeAdd :: T.Text -> GameContext.Attributes -> Maybe GameContext.Attributes
parseAttributeAdd input attrs = do
  (key, valStr) <- parseKeyValue input
  delta <- parseSignedDecimal valStr
  case key of
    "iron"   -> Just $ attrs { GameContext.iron = GameContext.iron attrs + delta }
    "edge"   -> Just $ attrs { GameContext.edge = GameContext.edge attrs + delta }
    "heart"  -> Just $ attrs { GameContext.heart = GameContext.heart attrs + delta }
    "shadow" -> Just $ attrs { GameContext.shadow = GameContext.shadow attrs + delta }
    "wits"   -> Just $ attrs { GameContext.wits = GameContext.wits attrs + delta }
    _        -> Nothing

-- | Avalia o resultado de um Action Roll (Ironsworn)
evaluateActionRoll :: Int -> Int -> Int -> Action.ActionRollResult
evaluateActionRoll actionDie challenge1 challenge2
  | actionDie > challenge1 && actionDie > challenge2 = Action.StrongHit
  | actionDie > challenge1 || actionDie > challenge2 = Action.WeakHit
  | otherwise = Action.Miss

-- | Converte ActionRollResult para string legível
showActionRollResult :: Action.ActionRollResult -> String
showActionRollResult Action.StrongHit = "STRONG HIT"
showActionRollResult Action.WeakHit = "WEAK HIT"
showActionRollResult Action.Miss = "MISS"
showActionRollResult Action.InvalidRoll = "INVALID ROLL"

-- | Funções auxiliares de parsing
parseKeyValue :: T.Text -> Maybe (T.Text, T.Text)
parseKeyValue input =
  case T.splitOn ":" input of
    [k, v] -> Just (k, v)
    _      -> Nothing

parseDecimal :: T.Text -> Maybe Int
parseDecimal valStr =
  case TR.decimal valStr of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

parseSignedDecimal :: T.Text -> Maybe Int
parseSignedDecimal valStr =
  case TR.signed TR.decimal valStr of
    Right (n, rest) | T.null rest -> Just n
    _ -> Nothing

-- | Limita um valor a um intervalo [low, high].
clamp :: Int -> Int -> Int -> Int
clamp low high x = max low (min high x)

-- | Verifica se uma consequência é um trigger de Pay the Price
isPayThePriceTrigger :: Move.Consequence -> Bool
isPayThePriceTrigger (Move.TriggerMove Move.PayThePrice) = True
isPayThePriceTrigger _ = False

-- | Parse string entre aspas duplas
-- Retorna (conteúdo, resto após as aspas)
parseQuotedString :: T.Text -> Maybe (T.Text, T.Text)
parseQuotedString input =
  case T.stripPrefix "\"" (T.stripStart input) of
    Nothing -> Nothing
    Just afterFirstQuote ->
      case T.breakOn "\"" afterFirstQuote of
        (content, rest) | not (T.null rest) ->
          Just (content, T.drop 1 rest)  -- Remove a segunda aspa
        _ -> Nothing

-- | Parse rank de challenge
parseRank :: T.Text -> Maybe Progress.ChallengeRank
parseRank text =
  case T.toLower . T.strip $ text of
    "troublesome" -> Just Progress.Troublesome
    "dangerous" -> Just Progress.Dangerous
    "formidable" -> Just Progress.Formidable
    "extreme" -> Just Progress.Extreme
    "epic" -> Just Progress.Epic
    _ -> Nothing

-- | Interpreta resultado de Fulfill Your Vow
interpretVowRoll :: (T.Text -> IO ()) -> Progress.ProgressRollResult -> IO ()
interpretVowRoll logMessage result = do
  logMessage "\n>>> Fulfill Your Vow <<<"
  case Progress.progressRollResult result of
    Action.StrongHit -> logMessage $ T.pack (C.vowStrongHit C.progressInterpretation)
    Action.WeakHit -> logMessage $ T.pack (C.vowWeakHit C.progressInterpretation)
    Action.Miss -> logMessage $ T.pack (C.vowMiss C.progressInterpretation)
    Action.InvalidRoll -> logMessage $ T.pack (C.rollError C.progressInterpretation)

-- | Interpreta resultado de End the Fight
interpretCombatRoll :: (T.Text -> IO ()) -> Progress.ProgressRollResult -> IO ()
interpretCombatRoll logMessage result = do
  logMessage "\n>>> End the Fight <<<"
  case Progress.progressRollResult result of
    Action.StrongHit -> logMessage $ T.pack (C.combatStrongHit C.progressInterpretation)
    Action.WeakHit -> logMessage $ T.pack (C.combatWeakHit C.progressInterpretation)
    Action.Miss -> logMessage $ T.pack (C.combatMiss C.progressInterpretation)
    Action.InvalidRoll -> logMessage $ T.pack (C.rollError C.progressInterpretation)

-- | Interpreta resultado de Reach Your Destination
interpretJourneyRoll :: (T.Text -> IO ()) -> Progress.ProgressRollResult -> IO ()
interpretJourneyRoll logMessage result = do
  logMessage "\n>>> Reach Your Destination <<<"
  case Progress.progressRollResult result of
    Action.StrongHit -> logMessage $ T.pack (C.journeyStrongHit C.progressInterpretation)
    Action.WeakHit -> logMessage $ T.pack (C.journeyWeakHit C.progressInterpretation)
    Action.Miss -> logMessage $ T.pack (C.journeyMiss C.progressInterpretation)
    Action.InvalidRoll -> logMessage $ T.pack (C.rollError C.progressInterpretation)