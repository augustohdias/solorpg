{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module System.Impl.ActionService (newHandle) where

import qualified System.ActionContract as Action
import System.ActionContract (ActionContext(..))
import qualified System.DiceContract as Dice
import qualified System.GameContextContract as GameContext
import qualified System.MoveContract as Move
import qualified System.ProgressContract as Progress
import qualified System.OracleContract as Oracle
import qualified System.HelpContract as Help
import qualified System.ConsequenceContract as Consequence
import System.ConsequenceContract (Consequence(..), Choice(..))
import qualified System.Constants as C
import qualified System.Util.Parser as Parser
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import System.Tui.Comm (GameOutput(..), MessageType(..))
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Monad (void, unless)
import Data.Foldable (for_)

newHandle :: Dice.Handle -> GameContext.Handle -> Move.Handle -> Progress.Handle -> Oracle.Handle -> Help.Handle -> TChan GameOutput -> IO Action.Handle
newHandle diceHandler contextHandler moveHandler progressHandler oracleHandler helpHandler tuiOutputChannel = do
  return actionHandle
  where
    actionContext = ActionContext {
      diceHandler = diceHandler,
      contextHandler = contextHandler,
      moveHandler = moveHandler,
      progressHandler = progressHandler,
      oracleHandler = oracleHandler,
      helpHandler = helpHandler,
      tuiOutputChannel = tuiOutputChannel
    }
    actionHandle = Action.Handle { Action.process = processAction actionContext }
    processAction aCtx actionType = case actionType of
      Action.AddStoryLog -> addStoryLog aCtx
      Action.RollDice -> rollDice aCtx actionHandle
      Action.Show -> showLogs aCtx
      Action.Exit -> exit aCtx
      Action.CreateCharacter -> createCharacter aCtx
      Action.LoadCharacter -> loadCharacter aCtx
      Action.ShowCharacter -> showCharacter aCtx
      Action.UpdateAttribute -> updateAttribute aCtx
      Action.UpdateResource -> updateResource aCtx
      Action.AddAttribute -> addAttribute aCtx
      Action.AddResource -> addResource aCtx
      Action.Challenge -> challenge aCtx actionHandle
      Action.Move -> moveAction aCtx actionHandle
      Action.SwearVow -> swearVow aCtx
      Action.MarkProgress -> markProgress aCtx
      Action.RollProgress -> rollProgress aCtx
      Action.ShowTracks -> showTracks aCtx
      Action.AbandonTrack -> abandonTrack aCtx
      Action.Oracle -> oracleQuery aCtx actionHandle
      Action.Help -> helpCommand aCtx
      Action.Bond -> bondCommand aCtx
      _ -> defaultAction aCtx

-- | Helper for sending narrative/game log messages to the TUI
logMessage :: ActionContext -> T.Text -> IO ()
logMessage ActionContext { tuiOutputChannel } msg =
  atomically $ writeTChan tuiOutputChannel (LogEntry msg NarrativeMessage)

-- | Helper for sending system notifications to the TUI
systemMessage :: ActionContext -> T.Text -> IO ()
systemMessage ActionContext { tuiOutputChannel } msg =
  atomically $ writeTChan tuiOutputChannel (LogEntry msg SystemMessage)

-- | Executa uma ação que requer um contexto de personagem carregado.
-- Mostra uma mensagem de erro se nenhum personagem estiver carregado.
withContext :: ActionContext -> (GameContext.Context -> IO Bool) -> IO Bool
withContext aCtx@ActionContext { contextHandler } action = do
  maybeCtx <- GameContext.getCurrentContext contextHandler
  case maybeCtx of
    Nothing -> do
      systemMessage aCtx (C.msgNoCharacterLoaded C.messages)
      return True
    Just ctx -> action ctx

-- | Executa uma ação em um contexto, se ele existir, mas não faz nada
-- (e não mostra erro) se o contexto for Nothing.
whenContext :: ActionContext -> (GameContext.Context -> IO ()) -> IO ()
whenContext ActionContext { contextHandler } action = do
  maybeCtx <- GameContext.getCurrentContext contextHandler
  for_ maybeCtx action

-- | Default action for unknown action types
defaultAction :: ActionContext -> T.Text -> IO Bool
defaultAction _ _ = return False

-- | Adiciona entrada narrativa ao log
addStoryLog :: ActionContext -> T.Text -> IO Bool
addStoryLog aCtx story = do
  if T.isPrefixOf ":" story
    then return True
    else do
      whenContext aCtx $ \ctx -> do
        ctxWithLog <- GameContext.addLogEntry (contextHandler aCtx) ctx story
        void $ GameContext.saveContext (contextHandler aCtx) ctxWithLog
      logMessage aCtx story
      return True

-- | Rola dados usando a string de especificação
rollDice :: ActionContext -> Action.Handle -> T.Text -> IO Bool
rollDice aCtx@ActionContext { diceHandler } actionH diceDescription = do
  rolls <- Dice.roll diceHandler diceDescription
  let msg = C.msgDiceRolled C.messages <> T.pack (show rolls)
  _ <- Action.process actionH Action.AddStoryLog msg
  logMessage aCtx "As tramas do destino foram lançadas..."
  return True

-- | Mostra logs da sessão
showLogs :: ActionContext -> T.Text -> IO Bool
showLogs aCtx _ = withContext aCtx $ \ctx -> do
  logMessage aCtx (C.msgLogsHeader C.messages)
  let logs = GameContext.getSessionLog (contextHandler aCtx) ctx
  mapM_ (logMessage aCtx) logs
  return True

-- | Encerra a sessão
exit :: ActionContext -> T.Text -> IO Bool
exit aCtx@ActionContext { tuiOutputChannel } _ = do
  systemMessage aCtx (C.msgSessionEnded C.messages)
  atomically $ writeTChan tuiOutputChannel GameEnd
  return False

-- | Cria novo personagem
createCharacter :: ActionContext -> T.Text -> IO Bool
createCharacter aCtx@ActionContext { contextHandler, tuiOutputChannel } input = do
  -- Input esperado: "NomePersonagem iron:3 edge:2 heart:2 shadow:1 wits:2"
  let parts = T.words input
  if null parts
    then do
      systemMessage aCtx (C.msgCharacterNameRequired C.messages)
      return True
    else do
      let charName = head parts
      let attrParts = tail parts
      let attrs = Parser.parseAttributes attrParts

      result <- GameContext.createContext contextHandler charName attrs
      case result of
        Left err -> do
          systemMessage aCtx (C.msgErrorCreating C.messages <> T.pack (show err))
          return True
        Right ctx -> do
          let msg = C.msgCharacterCreated C.messages <> GameContext.getCharacterName ctx
          systemMessage aCtx msg
          _ <- GameContext.saveContext contextHandler ctx
          atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter ctx))
          return True

-- | Carrega personagem existente
loadCharacter :: ActionContext -> T.Text -> IO Bool
loadCharacter aCtx@ActionContext { contextHandler, tuiOutputChannel } charName = do
  if T.null charName
    then do
      systemMessage aCtx (C.msgCharacterNameRequired C.messages)
      return True
    else do
      result <- GameContext.loadContext contextHandler charName
      case result of
        Left err -> do
          systemMessage aCtx (C.msgErrorLoading C.messages <> T.pack (show err))
          return True
        Right ctx -> do
          let msg = C.msgCharacterLoaded C.messages <> GameContext.getCharacterName ctx
          systemMessage aCtx msg
          let sessionLogs = reverse $ GameContext.getSessionLog contextHandler ctx
          mapM_ (logMessage aCtx) sessionLogs
          _ <- GameContext.saveContext contextHandler ctx
          atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter ctx))
          return True

-- | Mostra informações do personagem atual
showCharacter :: ActionContext -> T.Text -> IO Bool
showCharacter aCtx@ActionContext { tuiOutputChannel } _ = withContext aCtx $ \ctx -> do
  let char = GameContext.mainCharacter ctx
  atomically $ writeTChan tuiOutputChannel (CharacterUpdate char)
  return True

-- | Atualiza atributo (define valor absoluto)
updateAttribute :: ActionContext -> T.Text -> IO Bool
updateAttribute aCtx@ActionContext { contextHandler, tuiOutputChannel } input = withContext aCtx $ \ctx -> do
  let oldAttrs = GameContext.attributes (GameContext.mainCharacter ctx)
  case Parser.parseAttributeUpdate input oldAttrs of
    Nothing -> do
      systemMessage aCtx (C.msgInvalidAttributeFormat C.messages)
      return True
    Just newAttrs -> do
      updatedCtx <- GameContext.updateAttributes contextHandler ctx newAttrs
      _ <- GameContext.saveContext contextHandler updatedCtx
      systemMessage aCtx (C.msgAttributeUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Atualiza recurso (define valor absoluto)
updateResource :: ActionContext -> T.Text -> IO Bool
updateResource aCtx@ActionContext { contextHandler, tuiOutputChannel } input = withContext aCtx $ \ctx -> do
  let oldRes = GameContext.resources (GameContext.mainCharacter ctx)
  case Parser.parseResourceUpdate input oldRes of
    Nothing -> do
      systemMessage aCtx (C.msgInvalidResourceFormat C.messages)
      return True
    Just newRes -> do
      updatedCtx <- GameContext.updateResources contextHandler ctx newRes
      _ <- GameContext.saveContext contextHandler updatedCtx
      systemMessage aCtx (C.msgResourceUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Adiciona/remove atributo (valor delta)
addAttribute :: ActionContext -> T.Text -> IO Bool
addAttribute aCtx@ActionContext { contextHandler, tuiOutputChannel } input = withContext aCtx $ \ctx -> do
  let oldAttrs = GameContext.attributes (GameContext.mainCharacter ctx)
  case Parser.parseAttributeAdd input oldAttrs of
    Nothing -> do
      systemMessage aCtx "Formato inválido. Use: atributo:delta (ex: iron:-1, edge:+2)"
      return True
    Just newAttrs -> do
      updatedCtx <- GameContext.updateAttributes contextHandler ctx newAttrs
      _ <- GameContext.saveContext contextHandler updatedCtx
      systemMessage aCtx (C.msgAttributeUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Adiciona/remove recurso (valor delta)
addResource :: ActionContext -> T.Text -> IO Bool
addResource aCtx@ActionContext { contextHandler, tuiOutputChannel } input = withContext aCtx $ \ctx -> do
  let oldRes = GameContext.resources (GameContext.mainCharacter ctx)
  case Parser.parseResourceAdd input oldRes of
    Nothing -> do
      systemMessage aCtx "Formato inválido. Use: recurso:delta (ex: health:-1, momentum:+2)"
      return True
    Just newRes -> do
      updatedCtx <- GameContext.updateResources contextHandler ctx newRes
      _ <- GameContext.saveContext contextHandler updatedCtx
      systemMessage aCtx (C.msgResourceUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Rola 1d6+2d10 e avalia resultado (challenge roll simples)
challenge :: ActionContext -> Action.Handle -> T.Text -> IO Bool
challenge aCtx@ActionContext { diceHandler } actionH _ = do
  result <- Dice.challengeRoll diceHandler
  case result of
    Left err -> do
      logMessage aCtx err
      return True
    Right challengeResult -> do
      let formattedMsg = Dice.formatChallengeResult challengeResult
            (C.formatActionRoll C.characterDisplay)
            (C.challengeStrongHit C.challengeInterpretation)
            (C.challengeWeakHit C.challengeInterpretation)
            (C.challengeMiss C.challengeInterpretation)
            (C.challengeMatch C.challengeInterpretation)
      _ <- Action.process actionH Action.AddStoryLog formattedMsg
      return True

-- | Executa um move do Ironsworn
moveAction :: ActionContext -> Action.Handle -> T.Text -> IO Bool
moveAction aCtx@ActionContext { moveHandler } actionH input = withContext aCtx $ \ctx -> do
  let parts = T.words input
  case parts of
    [] -> do
      systemMessage aCtx $ T.pack (C.msgMoveUsage C.helpMessages)
      return True
    (moveName:params) ->
      case Move.parseMoveType moveHandler moveName of
        Nothing -> do
          systemMessage aCtx $ "Move desconhecido: " <> T.unwords parts <> "\n" <> T.pack (C.msgMovesAvailable C.helpMessages)
          return True
        Just moveType -> do
          let (maybeStat, _cleanParams) = parseMoveParams moveHandler params
          let char = GameContext.mainCharacter ctx
          let attrs = GameContext.attributes char
          let resources = GameContext.resources char

          -- Executa o move no MoveService (que faz a rolagem e retorna consequências)
          consequences <- Move.executeMove moveHandler moveType maybeStat attrs resources

          -- Processa as consequências através do ActionService
          processConsequences aCtx actionH consequences ctx
          return True

-- | Separa os parâmetros de um comando de move em stat (opcional) e o resto.
parseMoveParams :: Move.Handle -> [T.Text] -> (Maybe Move.Stat, T.Text)
parseMoveParams moveHandler params =
  case params of
    (first:rest) ->
      case Move.parseStat moveHandler first of
        Just s  -> (Just s, T.unwords rest)
        Nothing -> (Nothing, T.unwords params)
    [] -> (Nothing, "")

-- | Processa uma lista de consequências de um move ou oráculo
processConsequences :: ActionContext -> Action.Handle -> [Consequence] -> GameContext.Context -> IO ()
processConsequences aCtx actionH consequences ctx = do
  mapM_ (processConsequence aCtx actionH ctx) consequences

-- | Processa uma única consequência aplicando DRY
processConsequence :: ActionContext -> Action.Handle -> GameContext.Context -> Consequence -> IO ()
processConsequence aCtx@ActionContext { contextHandler } actionH ctx cons = case cons of
  Narrative text ->
    logMessage aCtx text

  LoseHealth amount ->
    applyResourceDelta aCtx "health" (-amount)

  LoseSpirit amount ->
    applyResourceDelta aCtx "spirit" (-amount)

  LoseSupply amount ->
    applyResourceDelta aCtx "supply" (-amount)

  LoseMomentum amount ->
    applyResourceDelta aCtx "momentum" (-amount)

  GainHealth amount ->
    applyResourceDelta aCtx "health" amount

  GainSpirit amount ->
    applyResourceDelta aCtx "spirit" amount

  GainSupply amount ->
    applyResourceDelta aCtx "supply" amount

  GainMomentum amount ->
    applyResourceDelta aCtx "momentum" amount

  TriggerMove nextMoveType -> do
    logMessage aCtx $ "\n>>> Executando " <> Consequence.moveTypeToText nextMoveType <> " automaticamente..."
    -- Recursivamente executa o próximo move usando o próprio ActionService
    let nextMoveText = T.toLower (Consequence.moveTypeToText nextMoveType)
    _ <- moveAction aCtx actionH nextMoveText
    return ()

  TriggerOracle oracleName -> do
    logMessage aCtx $ "\n🔮 Consultando oráculo " <> oracleName <> " automaticamente..."
    -- Executa oráculo através do ActionService
    _ <- oracleQuery aCtx actionH oracleName
    return ()

  PlayerChoice choices -> do
    maybeChoice <- Move.showChoices (moveHandler aCtx) choices
    case maybeChoice of
      Just choice -> do
        logMessage aCtx $ "\nVocê escolheu: " <> choiceDescription choice
        processConsequences aCtx actionH (choiceConsequences choice) ctx
      Nothing ->
        logMessage aCtx "Nenhuma escolha válida."

  AddBonus bonus -> do
    updatedCtx <- GameContext.addBonus contextHandler ctx bonus
    _ <- GameContext.saveContext contextHandler updatedCtx
    logMessage aCtx $ "Bônus adicionado: " <> GameContext.bonusDescription bonus <> " (+" <> T.pack (show (GameContext.bonusValue bonus)) <> ")"

-- | Aplica delta de recurso usando DRY - função auxiliar reutilizável
applyResourceDelta :: ActionContext -> T.Text -> Int -> IO ()
applyResourceDelta aCtx resourceName delta = do
  let deltaText = if delta >= 0
        then "+" <> T.pack (show delta)
        else T.pack (show delta)  -- já tem o sinal negativo
  let command = resourceName <> ":" <> deltaText
  _ <- addResource aCtx command
  return ()

-- | Consulta oráculo
oracleQuery :: ActionContext -> Action.Handle -> T.Text -> IO Bool
oracleQuery aCtx actionH input = do
  if T.null (T.strip input)
    then listOracles aCtx
    else queryOracle
  where
    queryOracle = do
      let (oracleName, maybeValue) = Parser.parseOracleQuery input
      if T.null maybeValue
        then rollOracleRandomly aCtx actionH oracleName
        else rollOracleWithGivenValue aCtx actionH oracleName maybeValue

    listOracles ActionContext { oracleHandler } = do
      oracles <- Oracle.listOracles oracleHandler
      if null oracles
        then logMessage aCtx "Nenhum oráculo carregado."
        else do
          logMessage aCtx "=== Oráculos Disponíveis ==="
          mapM_ (logMessage aCtx . ("• " <>)) oracles
      return True

rollOracleRandomly :: ActionContext -> Action.Handle -> T.Text -> IO Bool
rollOracleRandomly aCtx@ActionContext { oracleHandler } actionH oracleName = do
  result <- Oracle.rollOracle oracleHandler oracleName
  case result of
    Left err -> do
      systemMessage aCtx $ "Erro no oráculo: " <> T.pack (show err)
      return True
    Right oracleResult -> do
      let formattedResult = T.pack $
            "🔮 " ++ T.unpack (Oracle.resultOracle oracleResult) ++
            " (Rolou " ++ show (Oracle.resultRoll oracleResult) ++ "):\n" ++
            "→ " ++ T.unpack (Oracle.resultText oracleResult)

      -- Delega a adição do log para o handle de ação
      _ <- Action.process actionH Action.AddStoryLog formattedResult

      executeOracleConsequences aCtx actionH oracleResult
      return True

rollOracleWithGivenValue :: ActionContext -> Action.Handle -> T.Text -> T.Text -> IO Bool
rollOracleWithGivenValue aCtx@ActionContext { oracleHandler } actionH oracleName valueText = do
  case TR.decimal valueText of
    Right (val, _) -> do
      result <- Oracle.queryOracle oracleHandler oracleName val
      case result of
        Left err -> do
          systemMessage aCtx $ "Erro no oráculo: " <> T.pack (show err)
          return True
        Right oracleResult -> do
          let formattedResult = T.pack $
                "🔮 " ++ T.unpack (Oracle.resultOracle oracleResult) ++
                " (Índice " ++ show (Oracle.resultRoll oracleResult) ++ "):\n" ++
                "→ " ++ T.unpack (Oracle.resultText oracleResult)

          -- Delega a adição do log para o handle de ação
          _ <- Action.process actionH Action.AddStoryLog formattedResult

          -- Executa consequências estruturadas se houverem
          executeOracleConsequences aCtx actionH oracleResult
          return True
    Left _ -> do
      systemMessage aCtx "Valor inválido para consulta direta."
      return True

-- | Executa consequências de resultado de oráculo (estruturadas + backward compatibility)
executeOracleConsequences :: ActionContext -> Action.Handle -> Oracle.OracleResult -> IO ()
executeOracleConsequences aCtx@ActionContext { contextHandler } actionH result = do
  -- Primeiro, executa consequências estruturadas (novo sistema)
  let structuredConsequences = Oracle.resultConsequences result
  unless (null structuredConsequences) $ do
    maybeCtx <- GameContext.getCurrentContext contextHandler
    for_ maybeCtx $ \ctx ->
      processConsequences aCtx actionH structuredConsequences ctx

-- | Mostra ajuda
helpCommand :: ActionContext -> T.Text -> IO Bool
helpCommand aCtx@ActionContext { helpHandler } input = do
  let topic = T.strip input
  if T.null topic
    then do
      helpText <- Help.showHelp helpHandler
      logMessage aCtx (T.pack helpText)
    else case Help.parseTopic helpHandler topic of
      Just t -> do
        helpText <- Help.showTopicHelp helpHandler t
        logMessage aCtx (T.pack helpText)
      Nothing -> do
        logMessage aCtx $ "Tópico desconhecido: " <> topic
        logMessage aCtx "Tópicos disponíveis: moves, progress, oracle, chaining, character"
  return True

-- Placeholder functions for remaining actions  
swearVow, markProgress, rollProgress, showTracks, abandonTrack, bondCommand :: ActionContext -> T.Text -> IO Bool
swearVow _ _ = return True    -- TODO: Implement
markProgress _ _ = return True -- TODO: Implement
rollProgress _ _ = return True -- TODO: Implement
showTracks _ _ = return True   -- TODO: Implement
abandonTrack _ _ = return True -- TODO: Implement
bondCommand _ _ = return True  -- TODO: Implement