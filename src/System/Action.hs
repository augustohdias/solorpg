{-# LANGUAGE OverloadedStrings #-}

module System.Action
  ( ActionType (..),
    process,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar_)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Monad (unless, void, when)
import qualified Data.Aeson as Aeson
import Data.Char (isAlphaNum)
import Data.Foldable (forM_, for_)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Read as TR
import Data.Time.Clock (getCurrentTime)
import Network.Socket (PortNumber)
import System.ConsequenceContract (Choice (..), Consequence (..))
import qualified System.ConsequenceContract as Consequence
import qualified System.Constants as C
import qualified System.Dice as Dice
import System.GameContext (BondCommand (..))
import qualified System.GameContext as GameContext
import qualified System.Help as Help
import qualified System.Move as Move
import qualified System.Network.Client as NetworkClient
import qualified System.Network.NetworkState as NetworkState
import qualified System.Network.Protocol as NetworkProtocol
import qualified System.Network.Server as NetworkServer
import qualified System.Network.Types as NetworkTypes
import qualified System.Oracle as Oracle
import qualified System.Progress as Progress
import System.Random (randomIO)
import System.Tui.Comm
  ( ChoiceSelectionPayload (..),
    GameOutput (..),
    MessageType (..),
  )
import qualified System.Util.Parser as Parser

data ActionType
  = -- | Rola dados (ex: "3d6,2d10")
    RollDice
  | -- | Adiciona entrada narrativa ao log
    AddStoryLog
  | -- | Mostra logs da sessão
    Show
  | -- | Salva contexto atual
    Save
  | -- | Encerra sessão
    Exit
  | -- | Cria novo personagem (ex: "NomePersonagem")
    CreateCharacter
  | -- | Carrega personagem existente (ex: "NomePersonagem")
    LoadCharacter
  | -- | Mostra informações do personagem atual
    ShowCharacter
  | -- | Define atributo (ex: "iron:3")
    UpdateAttribute
  | -- | Define recurso (ex: "health:4")
    UpdateResource
  | -- | Adiciona/remove atributo (ex: "iron:-1")
    AddAttribute
  | -- | Adiciona/remove recurso (ex: "supply:-2")
    AddResource
  | -- | Rola 1d6,2d10 e avalia resultado
    Challenge
  | -- | Executa um Move de Ironsworn
    Move
  | -- | Jura um voto (cria progress track)
    SwearVow
  | -- | Cria um progress track de combate
    CreateCombatTrack
  | -- | Marca progresso em track
    MarkProgress
  | -- | Faz progress roll
    RollProgress
  | -- | Mostra todos os tracks ativos
    ShowTracks
  | -- | Abandona/remove um track
    AbandonTrack
  | -- | Consulta oráculo
    Oracle
  | -- | Mostra ajuda
    Help
  | -- | Gerencia bonds (vínculos)
    Bond
  | -- | Adiciona bônus de rolagem manualmente
    AddBonusManually
  | -- | Resolve uma escolha pendente enviada pela TUI
    ResolveChoice
  | -- | Inicia sessão multiplayer como host
    HostSession
  | -- | Conecta a uma sessão multiplayer
    ConnectSession
  | -- | Desconecta da sessão multiplayer
    DisconnectSession
  | -- | Cria um voto compartilhado
    SharedVow
  | -- | Aceita conexão pendente
    AcceptConnection
  | -- | Rejeita conexão pendente
    RejectConnection
  | -- | Ação desconhecida
    Unknown
  deriving (Show, Eq)

-- | Processa uma ação do jogo
process :: TChan GameOutput -> ActionType -> T.Text -> IO Bool
process tuiOutputChannel actionType input = case actionType of
  AddStoryLog -> addStoryLog tuiOutputChannel input
  RollDice -> rollDice tuiOutputChannel input
  Show -> showLogs tuiOutputChannel input
  Exit -> exit tuiOutputChannel input
  CreateCharacter -> createCharacter tuiOutputChannel input
  LoadCharacter -> loadCharacter tuiOutputChannel input
  ShowCharacter -> showCharacter tuiOutputChannel input
  UpdateAttribute -> updateAttribute tuiOutputChannel input
  UpdateResource -> updateResource tuiOutputChannel input
  AddAttribute -> addAttribute tuiOutputChannel input
  AddResource -> addResource tuiOutputChannel input
  Challenge -> challenge tuiOutputChannel input
  Move -> moveAction tuiOutputChannel input
  SwearVow -> swearVow tuiOutputChannel input
  CreateCombatTrack -> createCombatTrack tuiOutputChannel input
  MarkProgress -> markProgress tuiOutputChannel input
  RollProgress -> rollProgress tuiOutputChannel input
  ShowTracks -> showTracks tuiOutputChannel input
  AbandonTrack -> abandonTrack tuiOutputChannel input
  Bond -> bondCommand tuiOutputChannel input
  Oracle -> oracleQuery tuiOutputChannel input
  Help -> helpCommand tuiOutputChannel input
  AddBonusManually -> addBonusCommand tuiOutputChannel input
  ResolveChoice -> resolveChoice tuiOutputChannel input
  HostSession -> hostSession tuiOutputChannel input
  ConnectSession -> connectSession tuiOutputChannel input
  DisconnectSession -> disconnectSession tuiOutputChannel input
  SharedVow -> sharedVow tuiOutputChannel input
  AcceptConnection -> acceptConnection tuiOutputChannel input
  RejectConnection -> rejectConnection tuiOutputChannel input
  _ -> defaultAction tuiOutputChannel input

-- | Sends narrative messages to the TUI log channel.
logMessage :: TChan GameOutput -> T.Text -> IO ()
logMessage tuiOutputChannel msg =
  atomically $ writeTChan tuiOutputChannel (LogEntry msg NarrativeMessage)

-- | Sends system notifications to the TUI.
systemMessage :: TChan GameOutput -> T.Text -> IO ()
systemMessage tuiOutputChannel msg =
  atomically $ writeTChan tuiOutputChannel (LogEntry msg SystemMessage)

-- | Executes an action that requires a loaded character context.
withContext :: (GameContext.Context -> IO Bool) -> IO Bool
withContext action = do
  maybeCtx <- GameContext.getCurrentContext
  case maybeCtx of
    Nothing -> pure True
    Just ctx -> action ctx

-- | Conditionally executes an action with the current context if one exists.
whenContext :: (GameContext.Context -> IO ()) -> IO ()
whenContext action = do
  maybeCtx <- GameContext.getCurrentContext
  for_ maybeCtx action

-- | Default handler for unknown or unimplemented action types.
defaultAction :: TChan GameOutput -> T.Text -> IO Bool
defaultAction _ _ = return True

isFirstCharAlphaNum :: T.Text -> Bool
isFirstCharAlphaNum text =
  case T.uncons text of
    Nothing -> False
    Just (c, _) -> isAlphaNum c

-- | Adds a narrative entry to the character's session log.
addStoryLog :: TChan GameOutput -> T.Text -> IO Bool
addStoryLog tuiOutputChannel story
  | isCommand = return True
  | otherwise = execute
  where
    isCommand = T.isPrefixOf ":" story
    format ctx
      | not (isFirstCharAlphaNum story) = story
      | otherwise = GameContext.getCharacterName ctx <> ": " <> story
    execute = do
      whenContext $ \ctx -> do
        let formattedLog = format ctx
        ctxWithLog <- GameContext.addLogEntry ctx formattedLog
        void $ GameContext.saveContext ctxWithLog
        logMessage tuiOutputChannel formattedLog

        -- Se está em modo multiplayer e não é mensagem secreta (começa com ~)
        if GameContext.isMultiplayer ctx && not (T.isPrefixOf "~" formattedLog)
          then do
            networkState <- NetworkState.getNetworkState
            timestamp <- getCurrentTime
            let playerName = GameContext.getCharacterName ctx

            case networkState of
              NetworkState.ServerState serverState -> do
                -- Host faz broadcast para todos os clientes
                let msg = NetworkProtocol.StoryLogEntry playerName formattedLog timestamp
                NetworkServer.broadcastMessage serverState msg Nothing
              NetworkState.ClientState clientState -> do
                -- Cliente envia para o host
                let msg = NetworkProtocol.StoryLogEntry playerName formattedLog timestamp
                _ <- NetworkClient.sendMessage clientState msg
                return ()
              NetworkState.NoNetworkState -> return ()
          else return ()
      return True

-- | Sincroniza criação de shared vow
syncSharedVowCreated :: TChan GameOutput -> GameContext.Context -> T.Text -> Progress.ProgressTrack -> IO ()
syncSharedVowCreated tuiOutputChannel ctx vowName track = do
  networkState <- NetworkState.getNetworkState
  let playerName = GameContext.getCharacterName ctx
  let trackData = TE.decodeUtf8 $ BL.toStrict (Aeson.encode track)
  
  case networkState of
    NetworkState.ServerState serverState -> do
      let msg = NetworkProtocol.SharedVowCreated playerName vowName trackData
      NetworkServer.broadcastMessage serverState msg Nothing
    NetworkState.ClientState clientState -> do
      let msg = NetworkProtocol.SharedVowCreated playerName vowName trackData
      void $ NetworkClient.sendMessage clientState msg
    NetworkState.NoNetworkState -> return ()

-- | Sincroniza progresso em shared vow
syncSharedVowProgress :: TChan GameOutput -> GameContext.Context -> T.Text -> Int -> IO ()
syncSharedVowProgress tuiOutputChannel ctx vowName ticks = do
  networkState <- NetworkState.getNetworkState
  let playerName = GameContext.getCharacterName ctx
  
  case networkState of
    NetworkState.ServerState serverState -> do
      let msg = NetworkProtocol.SharedVowProgress playerName vowName ticks
      NetworkServer.broadcastMessage serverState msg Nothing
    NetworkState.ClientState clientState -> do
      let msg = NetworkProtocol.SharedVowProgress playerName vowName ticks
      void $ NetworkClient.sendMessage clientState msg
    NetworkState.NoNetworkState -> return ()

-- | Sincroniza conclusão de shared vow
syncSharedVowCompleted :: TChan GameOutput -> GameContext.Context -> T.Text -> Int -> IO ()
syncSharedVowCompleted tuiOutputChannel ctx vowName experience = do
  networkState <- NetworkState.getNetworkState
  let playerName = GameContext.getCharacterName ctx
  
  case networkState of
    NetworkState.ServerState serverState -> do
      let msg = NetworkProtocol.SharedVowCompleted playerName vowName experience
      NetworkServer.broadcastMessage serverState msg Nothing
    NetworkState.ClientState clientState -> do
      let msg = NetworkProtocol.SharedVowCompleted playerName vowName experience
      void $ NetworkClient.sendMessage clientState msg
    NetworkState.NoNetworkState -> return ()

-- | Executes dice rolls using dice specification strings.
rollDice :: TChan GameOutput -> T.Text -> IO Bool
rollDice tuiOutputChannel diceDescription = do
  rolls <- Dice.roll diceDescription
  let msg = C.msgDiceRolled C.messages <> T.pack (show rolls)
  _ <- process tuiOutputChannel AddStoryLog msg
  return True

-- | Displays the current character's session logs.
showLogs :: TChan GameOutput -> T.Text -> IO Bool
showLogs tuiOutputChannel _ = withContext $ \ctx -> do
  systemMessage tuiOutputChannel (C.msgLogsHeader C.messages)
  let logs = GameContext.getSessionLog ctx
  mapM_ (logMessage tuiOutputChannel) logs
  return True

-- | Terminates the current game session.
exit :: TChan GameOutput -> T.Text -> IO Bool
exit tuiOutputChannel _ = do
  systemMessage tuiOutputChannel (C.msgSessionEnded C.messages)
  atomically $ writeTChan tuiOutputChannel GameEnd
  return False

-- | Creates a new character with specified name and attributes.
createCharacter :: TChan GameOutput -> T.Text -> IO Bool
createCharacter tuiOutputChannel input = do
  let parts = T.words input
  case parts of
    [] -> systemMessage tuiOutputChannel (C.msgCharacterNameRequired C.messages) >> return True
    (charName : attrParts) -> do
      let attrs = Parser.parseAttributes attrParts
      result <- GameContext.createContext charName attrs
      either handleError handleSuccess result
  where
    handleError err = systemMessage tuiOutputChannel (C.msgErrorCreating C.messages <> T.pack (show err)) >> return True
    handleSuccess ctx = do
      let msg = C.msgCharacterCreated C.messages <> GameContext.getCharacterName ctx
      systemMessage tuiOutputChannel msg
      _ <- GameContext.saveContext ctx
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter ctx))
      return True

-- | Loads an existing character from persistent storage.
loadCharacter :: TChan GameOutput -> T.Text -> IO Bool
loadCharacter tuiOutputChannel charName
  | T.null charName = systemMessage tuiOutputChannel (C.msgCharacterNameRequired C.messages) >> return True
  | otherwise = do
      result <- GameContext.loadContext charName
      either handleError handleSuccess result
  where
    handleError err = systemMessage tuiOutputChannel (C.msgErrorLoading C.messages <> T.pack (show err)) >> return True
    handleSuccess ctx = do
      let msg = C.msgCharacterLoaded C.messages <> GameContext.getCharacterName ctx
      systemMessage tuiOutputChannel msg
      let sessionLogs = reverse $ GameContext.getSessionLog ctx
      mapM_ (logMessage tuiOutputChannel) sessionLogs
      _ <- GameContext.saveContext ctx
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter ctx))
      return True

-- | Displays current character information in the TUI.
showCharacter :: TChan GameOutput -> T.Text -> IO Bool
showCharacter tuiOutputChannel _ = withContext $ \ctx -> do
  let char = GameContext.mainCharacter ctx
  atomically $ writeTChan tuiOutputChannel (CharacterUpdate char)
  return True

-- | Updates a character attribute to an absolute value.
updateAttribute :: TChan GameOutput -> T.Text -> IO Bool
updateAttribute tuiOutputChannel input = withContext $ \ctx -> do
  let oldAttrs = GameContext.attributes (GameContext.mainCharacter ctx)
  maybe handleParseError (handleUpdate ctx) (Parser.parseAttributeUpdate input oldAttrs)
  where
    handleParseError = systemMessage tuiOutputChannel (C.msgInvalidAttributeFormat C.messages) >> return True
    handleUpdate ctx newAttrs = do
      updatedCtx <- GameContext.updateAttributes ctx newAttrs
      _ <- GameContext.saveContext updatedCtx
      systemMessage tuiOutputChannel (C.msgAttributeUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Updates a character resource to an absolute value.
updateResource :: TChan GameOutput -> T.Text -> IO Bool
updateResource tuiOutputChannel input = withContext $ \ctx -> do
  let oldRes = GameContext.resources (GameContext.mainCharacter ctx)
  maybe handleParseError (handleUpdate ctx) (Parser.parseResourceUpdate input oldRes)
  where
    handleParseError = systemMessage tuiOutputChannel (C.msgInvalidResourceFormat C.messages) >> return True
    handleUpdate ctx newRes = do
      updatedCtx <- GameContext.updateResources ctx newRes
      _ <- GameContext.saveContext updatedCtx
      systemMessage tuiOutputChannel (C.msgResourceUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Modifies a character attribute by a delta value.
addAttribute :: TChan GameOutput -> T.Text -> IO Bool
addAttribute tuiOutputChannel input = withContext $ \ctx -> do
  let oldAttrs = GameContext.attributes (GameContext.mainCharacter ctx)
  maybe handleParseError (handleUpdate ctx) (Parser.parseAttributeAdd input oldAttrs)
  where
    handleParseError = systemMessage tuiOutputChannel "Formato inválido. Use: atributo:delta (ex: iron:-1, edge:+2)" >> return True
    handleUpdate ctx newAttrs = do
      updatedCtx <- GameContext.updateAttributes ctx newAttrs
      _ <- GameContext.saveContext updatedCtx
      systemMessage tuiOutputChannel (C.msgAttributeUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Modifies a character resource by a delta value.
addResource :: TChan GameOutput -> T.Text -> IO Bool
addResource tuiOutputChannel input = withContext $ \ctx -> do
  let oldRes = GameContext.resources (GameContext.mainCharacter ctx)
  maybe handleParseError (handleUpdate ctx) (Parser.parseResourceAdd input oldRes)
  where
    handleParseError = systemMessage tuiOutputChannel "Formato inválido. Use: recurso:delta (ex: health:-1, momentum:+2)" >> return True
    handleUpdate ctx newRes = do
      updatedCtx <- GameContext.updateResources ctx newRes
      _ <- GameContext.saveContext updatedCtx
      systemMessage tuiOutputChannel (C.msgResourceUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Executes an Ironsworn challenge roll (1d6 + 2d10).
challenge :: TChan GameOutput -> T.Text -> IO Bool
challenge tuiOutputChannel _ = do
  result <- Dice.challengeRoll
  either handleError handleSuccess result
  where
    handleError err = systemMessage tuiOutputChannel err >> return True
    handleSuccess challengeResult = do
      let formattedMsg =
            Dice.formatChallengeResult
              challengeResult
              (C.formatActionRoll C.characterDisplay)
              (C.challengeStrongHit C.challengeInterpretation)
              (C.challengeWeakHit C.challengeInterpretation)
              (C.challengeMiss C.challengeInterpretation)
              (C.challengeMatch C.challengeInterpretation)
      _ <- process tuiOutputChannel AddStoryLog formattedMsg
      return True

-- | Executes an Ironsworn move with optional stat modifier.
moveAction :: TChan GameOutput -> T.Text -> IO Bool
moveAction tuiOutputChannel input = withContext $ \ctx -> do
  let parts = T.words input
  case parts of
    [] -> systemMessage tuiOutputChannel (T.pack (C.msgMoveUsage C.helpMessages)) >> return True
    (moveName : params) ->
      case Move.parseMoveType moveName of
        Nothing -> do
          systemMessage tuiOutputChannel $ "Move desconhecido: " <> T.unwords parts <> "\n" <> T.pack (C.msgMovesAvailable C.helpMessages)
          return True
        Just moveType -> do
          let (maybeStat, _cleanParams) = parseMoveParams params
          let char = GameContext.mainCharacter ctx
          let attrs = GameContext.attributes char
          let resources = GameContext.resources char
          consequences <- Move.executeMove moveType maybeStat attrs resources
          processConsequences tuiOutputChannel consequences ctx >> return True

-- | Parses move command parameters into optional stat and remaining text.
parseMoveParams :: [T.Text] -> (Maybe Move.Stat, T.Text)
parseMoveParams [] = (Nothing, "")
parseMoveParams params@(first : rest) = (stat, text)
  where
    stat = Move.parseStat first
    text = maybe (T.unwords params) (const (T.unwords rest)) stat

-- | Processes a list of consequences from moves or oracle results.
processConsequences :: TChan GameOutput -> [Consequence] -> GameContext.Context -> IO ()
processConsequences _ [] _ = pure ()
processConsequences tuiOutputChannel (current : rest) ctx = do
  shouldPause <- processConsequence tuiOutputChannel ctx current rest
  unless shouldPause (processConsequences tuiOutputChannel rest ctx)

-- | Processes a single consequence using the recursive action architecture.
processConsequence :: TChan GameOutput -> GameContext.Context -> Consequence -> [Consequence] -> IO Bool
processConsequence tuiOutputChannel ctx cons remaining = case cons of
  Narrative text -> do
    void $ process tuiOutputChannel AddStoryLog text
    pure False
  LoseHealth amount ->
    applyResourceDelta tuiOutputChannel "health" (-amount) >> pure False
  LoseSpirit amount ->
    applyResourceDelta tuiOutputChannel "spirit" (-amount) >> pure False
  LoseSupply amount ->
    applyResourceDelta tuiOutputChannel "supply" (-amount) >> pure False
  LoseMomentum amount ->
    applyResourceDelta tuiOutputChannel "momentum" (-amount) >> pure False
  GainHealth amount ->
    applyResourceDelta tuiOutputChannel "health" amount >> pure False
  GainSpirit amount ->
    applyResourceDelta tuiOutputChannel "spirit" amount >> pure False
  GainSupply amount ->
    applyResourceDelta tuiOutputChannel "supply" amount >> pure False
  GainMomentum amount ->
    applyResourceDelta tuiOutputChannel "momentum" amount >> pure False
  TriggerMove nextMoveType -> do
    logMessage tuiOutputChannel $ "\n>>> Executando " <> Consequence.moveTypeToText nextMoveType <> " automaticamente..."
    let nextMoveText = T.toLower (Consequence.moveTypeToText nextMoveType)
    _ <- process tuiOutputChannel Move nextMoveText
    return False
  TriggerOracle oracleName -> do
    logMessage tuiOutputChannel $ "\n[*] Consultando oráculo " <> oracleName <> " automaticamente..."
    _ <- process tuiOutputChannel Oracle oracleName
    return False
  PlayerChoice choices ->
    presentPlayerChoice tuiOutputChannel choices remaining >> return True
  Consequence.AddBonus bonus -> do
    updatedCtx <- GameContext.addBonus ctx bonus
    _ <- GameContext.saveContext updatedCtx
    systemMessage tuiOutputChannel $ "Bônus adicionado: " <> GameContext.bonusDescription bonus <> " (+" <> T.pack (show (GameContext.bonusValue bonus)) <> ")"
    return False
  MarkBondProgress -> do
    markBondProgressTrack tuiOutputChannel ctx
    return False

-- | Sends a structured choice prompt to the TUI.
presentPlayerChoice :: TChan GameOutput -> [Choice] -> [Consequence] -> IO ()
presentPlayerChoice tuiOutputChannel choices remaining = do
  promptIdSeed <- randomIO :: IO Int
  let payload = Parser.buildChoicePromptPayload promptIdSeed choices remaining
  atomically $ writeTChan tuiOutputChannel (ChoicePrompt payload)
  systemMessage tuiOutputChannel "Escolha uma opção para continuar o movimento."

-- | Handles the command that resolves a pending player choice sent from the TUI.
resolveChoice :: TChan GameOutput -> T.Text -> IO Bool
resolveChoice tuiOutputChannel rawInput =
  case Aeson.eitherDecodeStrict (TE.encodeUtf8 rawInput) of
    Left err -> do
      systemMessage tuiOutputChannel $ "Erro ao interpretar escolha: " <> T.pack err
      return True
    Right selection ->
      if choiceSelectionCancelled selection
        then do
          systemMessage tuiOutputChannel "Escolha cancelada."
          return True
        else do
          maybeCtx <- GameContext.getCurrentContext
          case maybeCtx of
            Nothing -> do
              systemMessage tuiOutputChannel "Nenhum personagem carregado. Não é possível aplicar a escolha."
              return True
            Just ctx -> do
              let desc = choiceSelectionLabel selection
              unless (T.null desc) $
                logMessage tuiOutputChannel $
                  "\nVocê escolheu: " <> desc
              processConsequences tuiOutputChannel (choiceSelectionConsequences selection) ctx
              return True

-- | Marks progress on the bonds track when Forge a Bond succeeds.
markBondProgressTrack :: TChan GameOutput -> GameContext.Context -> IO ()
markBondProgressTrack tuiOutputChannel _ctx = do
  maybeCurrentCtx <- GameContext.getCurrentContext
  case maybeCurrentCtx of
    Nothing -> return ()
    Just currentCtx -> do
      let bondTrackName = "Bonds"
      let maybeTrack = GameContext.getProgressTrack currentCtx bondTrackName

      (track, wasCreated) <- case maybeTrack of
        Just existingTrack -> return (existingTrack, False)
        Nothing -> do
          let newTrack = Progress.newProgressTrack bondTrackName Progress.Bond Progress.Troublesome
          return (newTrack, True)

      updatedTrack <- Progress.markProgressTicks track 1

      updatedCtx <-
        if wasCreated
          then GameContext.addProgressTrack currentCtx updatedTrack
          else GameContext.updateProgressTrack currentCtx bondTrackName updatedTrack

      _ <- GameContext.saveContext updatedCtx

      let boxes = Progress.getProgressScore updatedTrack
      let ticks = Progress.trackTicks updatedTrack
      logMessage tuiOutputChannel $ "[+] Bond progress: " <> T.pack (show boxes) <> "/10 boxes (" <> T.pack (show ticks) <> "/40 ticks)"

-- | Applies a delta change to character resources through Action.process.
applyResourceDelta :: TChan GameOutput -> T.Text -> Int -> IO ()
applyResourceDelta tuiOutputChannel resourceName delta = do
  let command = resourceName <> ":" <> deltaText
  void (process tuiOutputChannel AddResource command)
  where
    deltaText
      | delta >= 0 = "+" <> T.pack (show delta)
      | otherwise = T.pack (show delta)

-- | Queries oracle systems with optional specific values.
oracleQuery :: TChan GameOutput -> T.Text -> IO Bool
oracleQuery tuiOutputChannel input = do
  if T.null (T.strip input)
    then listOracles tuiOutputChannel
    else queryOracle
  where
    queryOracle = do
      let (oracleName, maybeValue) = Parser.parseOracleQuery input
      if T.null maybeValue
        then rollOracleRandomly tuiOutputChannel oracleName
        else rollOracleWithGivenValue tuiOutputChannel oracleName maybeValue

    listOracles outputChan = do
      oracles <- Oracle.listOracles
      if null oracles
        then systemMessage outputChan "Nenhum oráculo carregado."
        else do
          systemMessage outputChan "=== Oráculos Disponíveis ==="
          mapM_ (systemMessage outputChan . ("• " <>)) oracles
      return True

-- | Executes a random oracle roll and processes the result.
rollOracleRandomly :: TChan GameOutput -> T.Text -> IO Bool
rollOracleRandomly tuiOutputChannel oracleName = do
  result <- Oracle.rollOracle oracleName
  case result of
    Left err -> do
      systemMessage tuiOutputChannel $ "Erro no oráculo: " <> T.pack (show err)
      return True
    Right oracleResult -> do
      let formattedResult =
            T.pack $
              "[*] Oráculo: "
                ++ T.unpack (Oracle.resultOracle oracleResult)
                ++ " (Rolou "
                ++ show (Oracle.resultRoll oracleResult)
                ++ "):\n"
                ++ "-> "
                ++ T.unpack (Oracle.resultText oracleResult)

      _ <- process tuiOutputChannel AddStoryLog formattedResult
      executeOracleConsequences tuiOutputChannel oracleResult
      return True

-- | Executes an oracle query with a specific index value.
rollOracleWithGivenValue :: TChan GameOutput -> T.Text -> T.Text -> IO Bool
rollOracleWithGivenValue tuiOutputChannel oracleName valueText = do
  case TR.decimal valueText of
    Right (val, _) -> do
      result <- Oracle.queryOracle oracleName val
      case result of
        Left err -> do
          systemMessage tuiOutputChannel $ "Erro no oráculo: " <> T.pack (show err)
          return True
        Right oracleResult -> do
          let formattedResult =
                T.pack $
                  "[*] Oráculo: "
                    ++ T.unpack (Oracle.resultOracle oracleResult)
                    ++ " (Índice "
                    ++ show (Oracle.resultRoll oracleResult)
                    ++ "):\n"
                    ++ "-> "
                    ++ T.unpack (Oracle.resultText oracleResult)

          _ <- process tuiOutputChannel AddStoryLog formattedResult
          executeOracleConsequences tuiOutputChannel oracleResult
          return True
    Left _ -> do
      systemMessage tuiOutputChannel "Valor inválido para consulta direta."
      return True

-- | Executes structured consequences from oracle results.
executeOracleConsequences :: TChan GameOutput -> Oracle.OracleResult -> IO ()
executeOracleConsequences tuiOutputChannel result = do
  let structuredConsequences = Oracle.resultConsequences result
  unless (null structuredConsequences) $ do
    maybeCtx <- GameContext.getCurrentContext
    for_ maybeCtx $ \ctx ->
      processConsequences tuiOutputChannel structuredConsequences ctx

-- | Displays help information for game commands and topics.
helpCommand :: TChan GameOutput -> T.Text -> IO Bool
helpCommand tuiOutputChannel input = do
  let topic = T.strip input
  if T.null topic
    then do
      helpText <- Help.showHelp "help"
      systemMessage tuiOutputChannel (T.pack helpText)
    else case Help.parseTopic topic of
      Just t -> do
        helpText <- Help.showTopicHelp "help" t
        systemMessage tuiOutputChannel (T.pack helpText)
      Nothing -> do
        systemMessage tuiOutputChannel $ "Tópico desconhecido: " <> topic
        systemMessage tuiOutputChannel "Tópicos disponíveis: moves, progress, oracle, chaining, character"
  return True

-- | Creates a new vow (Ironsworn progress track).
swearVow :: TChan GameOutput -> T.Text -> IO Bool
swearVow tuiOutputChannel input = withContext $ \ctx -> do
  maybe handleParseError (processVow ctx) (Parser.parseQuotedString input)
  where
    handleParseError = systemMessage tuiOutputChannel (T.pack (C.msgVowUsage C.moveMessages)) >> return True
    processVow ctx (vowName, rest) = maybe (handleInvalidRank rest) (createVow ctx vowName) (Parser.parseRank (T.strip rest))
    handleInvalidRank rest = do
      systemMessage tuiOutputChannel $ T.pack (C.errInvalidRank C.errorMessages) <> T.strip rest
      systemMessage tuiOutputChannel $ T.pack (C.msgVowUsage C.moveMessages)
      return True
    createVow ctx vowName rank = do
      let track = Progress.newProgressTrack vowName Progress.Vow rank
      let ticks = Progress.getTicksForRank rank
      updatedCtx <- GameContext.addProgressTrack ctx track
      _ <- GameContext.saveContext updatedCtx
      let formattedMsg = T.pack $ C.formatVowCreated C.moveMessages vowName (T.unpack $ Parser.rankToText rank) ticks
      _ <- process tuiOutputChannel AddStoryLog formattedMsg
      return True

-- | Creates a new combat progress track.
createCombatTrack :: TChan GameOutput -> T.Text -> IO Bool
createCombatTrack tuiOutputChannel input = withContext $ \ctx -> do
  maybe handleParseError (processCombat ctx) (Parser.parseQuotedString input)
  where
    handleParseError = systemMessage tuiOutputChannel (T.pack (C.msgCombatTrackUsage C.moveMessages)) >> return True
    processCombat ctx (enemyName, rest) = maybe (handleInvalidRank rest) (createCombat ctx enemyName) (Parser.parseRank (T.strip rest))
    handleInvalidRank rest = do
      systemMessage tuiOutputChannel $ T.pack (C.errInvalidRank C.errorMessages) <> T.strip rest
      systemMessage tuiOutputChannel $ T.pack (C.msgCombatTrackUsage C.moveMessages)
      return True
    createCombat ctx enemyName rank = do
      let track = Progress.newProgressTrack enemyName Progress.Combat rank
      let ticks = Progress.getTicksForRank rank
      updatedCtx <- GameContext.addProgressTrack ctx track
      _ <- GameContext.saveContext updatedCtx
      let formattedMsg = T.pack $ C.formatCombatTrackCreated C.moveMessages enemyName (T.unpack $ Parser.rankToText rank) ticks
      _ <- process tuiOutputChannel AddStoryLog formattedMsg
      
      -- Sincroniza track de combate em modo multiplayer
      when (GameContext.isMultiplayer updatedCtx) $ do
        syncProgressTrack tuiOutputChannel updatedCtx enemyName track
      
      return True

-- | Marks progress on an existing progress track.
markProgress :: TChan GameOutput -> T.Text -> IO Bool
markProgress tuiOutputChannel input = withContext $ \ctx -> do
  let trackName = T.strip input
  case nonEmpty trackName of
    Nothing -> systemMessage tuiOutputChannel (T.pack (C.msgProgressUsage C.moveMessages)) >> return True
    Just name -> handleTrackName ctx name
  where
    handleTrackName ctx name = case GameContext.getProgressTrack ctx name of
      Nothing -> systemMessage tuiOutputChannel (C.msgTrackNotFound C.moveMessages <> name) >> return True
      Just track -> processTrack ctx name track

    processTrack ctx name track
      | Progress.trackCompleted track = systemMessage tuiOutputChannel "Track já está completo!" >> return True
      | otherwise = do
          updatedTrack <- Progress.markProgress track
          updatedCtx <- GameContext.updateProgressTrack ctx name updatedTrack
          _ <- GameContext.saveContext updatedCtx
          let boxes = Progress.getProgressScore updatedTrack
          let ticks = Progress.trackTicks updatedTrack
          let msg =
                "[+] Progresso marcado: "
                  <> name
                  <> " ("
                  <> T.pack (show boxes)
                  <> "/10 boxes, "
                  <> T.pack (show ticks)
                  <> "/40 ticks)"
          _ <- process tuiOutputChannel AddStoryLog msg
          
          -- Sincroniza track em modo multiplayer
          when (GameContext.isMultiplayer updatedCtx) $ do
            case Progress.trackType track of
              Progress.Combat -> syncProgressTrack tuiOutputChannel updatedCtx name updatedTrack
              Progress.Journey -> syncProgressTrack tuiOutputChannel updatedCtx name updatedTrack
              Progress.Vow -> do
                -- Verifica se é shared vow (todos os vows em multiplayer são compartilhados)
                let oldTicks = Progress.trackTicks track
                let newTicks = Progress.trackTicks updatedTrack
                when (newTicks > oldTicks) $ do
                  syncSharedVowProgress tuiOutputChannel updatedCtx name (newTicks - oldTicks)
              _ -> return ()
          
          return True

    nonEmpty s = if T.null s then Nothing else Just s

-- | Executes a progress roll to attempt completing a progress track.
rollProgress :: TChan GameOutput -> T.Text -> IO Bool
rollProgress tuiOutputChannel input = withContext $ \ctx -> do
  let trackName = T.strip input
  case nonEmpty trackName of
    Nothing -> systemMessage tuiOutputChannel "Uso: :fulfill \"<nome do track>\"" >> return True
    Just name -> handleTrackName ctx name
  where
    handleTrackName ctx name = case GameContext.getProgressTrack ctx name of
      Nothing -> systemMessage tuiOutputChannel (C.msgTrackNotFound C.moveMessages <> name) >> return True
      Just track -> processTrack ctx name track

    processTrack ctx name track
      | Progress.trackCompleted track = systemMessage tuiOutputChannel "Track já está completo!" >> return True
      | otherwise = do
          executionResult <- Progress.executeProgressRoll track
          _ <- process tuiOutputChannel AddStoryLog (Progress.executionNarrativeMessage executionResult)
          updatedCtx <- GameContext.updateProgressTrack ctx name (Progress.executionUpdatedTrack executionResult)
          finalCtx <- applyExperienceGain updatedCtx (Progress.executionExperienceGained executionResult)
          _ <- GameContext.saveContext finalCtx
          forM_ (Progress.executionSystemMessage executionResult) (systemMessage tuiOutputChannel)
          
          -- Sincroniza shared vow quando completado
          when (GameContext.isMultiplayer finalCtx && Progress.trackType track == Progress.Vow && Progress.executionExperienceGained executionResult > 0) $ do
            syncSharedVowCompleted tuiOutputChannel finalCtx name (Progress.executionExperienceGained executionResult)
            -- Host também recebe experiência automaticamente (clientes recebem via broadcast)
          
          return True

    applyExperienceGain ctx expGained
      | expGained > 0 = do
          let command = "experience:+" <> T.pack (show expGained)
          _ <- process tuiOutputChannel AddResource command
          fromMaybe ctx <$> GameContext.getCurrentContext
      | otherwise = return ctx

    nonEmpty s = if T.null s then Nothing else Just s

-- | Displays all active progress tracks.
showTracks :: TChan GameOutput -> T.Text -> IO Bool
showTracks tuiOutputChannel _ = withContext $ \ctx -> do
  let tracks = GameContext.progressTracks ctx
  if null tracks
    then do
      systemMessage tuiOutputChannel $ T.pack (C.msgNoTracksActive C.moveMessages)
    else do
      systemMessage tuiOutputChannel $ T.pack (C.msgTracksHeader C.moveMessages)
      mapM_ (systemMessage tuiOutputChannel . Parser.formatProgressTrack) tracks
  return True

-- | Abandons and removes a progress track from the character context.
abandonTrack :: TChan GameOutput -> T.Text -> IO Bool
abandonTrack tuiOutputChannel input = withContext $ \ctx -> do
  let trackName = T.strip input
  case nonEmpty trackName of
    Nothing -> systemMessage tuiOutputChannel "Uso: :abandon \"<nome do track>\"" >> return True
    Just name -> handleTrackName ctx name
  where
    handleTrackName ctx name = case GameContext.getProgressTrack ctx name of
      Nothing -> systemMessage tuiOutputChannel (C.msgTrackNotFound C.moveMessages <> name) >> return True
      Just _track -> removeTrack ctx name

    removeTrack ctx name = do
      updatedCtx <- GameContext.removeProgressTrack ctx name
      _ <- GameContext.saveContext updatedCtx
      let msg = C.msgTrackRemoved C.moveMessages <> name
      _ <- process tuiOutputChannel AddStoryLog msg
      return True

    nonEmpty s = if T.null s then Nothing else Just s

-- | Manages character bonds (relationships with people, communities, and places).
bondCommand :: TChan GameOutput -> T.Text -> IO Bool
bondCommand tuiOutputChannel input = do
  _ <- processBondCommand maybeValidCommand >>= logCommandResponse maybeValidCommand
  return True
  where
    maybeValidCommand = Parser.parseBondCommand input

    processBondCommand Nothing = pure $ Left GameContext.InvalidCommand
    processBondCommand (Just validCommand) = GameContext.processBondCommand validCommand

    logCommandResponse :: Maybe GameContext.BondCommand -> Either GameContext.ContextError GameContext.BondProcessingResponse -> IO ()
    logCommandResponse (Just GameContext.BondCommand {bondCommandType = (GameContext.UpdateBondNotes _)}) (Right r) = logMessage tuiOutputChannel (r & GameContext.systemMessage)
    logCommandResponse _ (Left (GameContext.FileError msg)) = systemMessage tuiOutputChannel $ "Erro ao processar comando de bond: " <> T.pack msg
    logCommandResponse _ (Left _) = systemMessage tuiOutputChannel "Erro desconhecido ao processar comando de bond."
    logCommandResponse _ (Right r) = systemMessage tuiOutputChannel (r & GameContext.systemMessage)

-- | Adds a bonus to the character's active bonuses.
addBonusCommand :: TChan GameOutput -> T.Text -> IO Bool
addBonusCommand tuiOutputChannel input = withContext $ \ctx -> do
  case Parser.parseBonusCommand input of
    Nothing -> do
      systemMessage tuiOutputChannel "Formato inválido. Use: :bonus <tipo> <valor> [descrição]"
      systemMessage tuiOutputChannel "Tipos: nextroll, nextmove:<nome>, persistent"
      systemMessage tuiOutputChannel "Exemplos:"
      systemMessage tuiOutputChannel "  :bonus nextroll +1 \"Preparado\""
      systemMessage tuiOutputChannel "  :bonus nextmove:FaceDanger +2 \"Vantagem\""
      systemMessage tuiOutputChannel "  :bonus persistent +1 \"Bônus permanente\""
      return True
    Just bonus -> do
      updatedCtx <- GameContext.addBonus ctx bonus
      _ <- GameContext.saveContext updatedCtx
      let bonusTypeStr = case GameContext.bonusType bonus of
            GameContext.NextRoll -> "próxima rolagem"
            GameContext.NextMove moveName -> "próximo move: " <> moveName
            GameContext.Persistent -> "permanente"
      systemMessage tuiOutputChannel $
        "Bônus adicionado: "
          <> GameContext.bonusDescription bonus
          <> " (+"
          <> T.pack (show (GameContext.bonusValue bonus))
          <> ") para "
          <> bonusTypeStr
      return True

-- | Inicia uma sessão multiplayer como host
hostSession :: TChan GameOutput -> T.Text -> IO Bool
hostSession tuiOutputChannel _input = do
  maybeCtx <- GameContext.getCurrentContext
  case maybeCtx of
    Nothing -> do
      systemMessage tuiOutputChannel "Erro: Nenhum personagem carregado. Use :load para carregar um personagem."
      return True
    Just ctx -> do
      let playerName = GameContext.getCharacterName ctx
      let characterName = GameContext.name (GameContext.mainCharacter ctx)

      -- Inicia servidor com canal TUI
      result <- NetworkServer.startServer NetworkServer.defaultPort playerName characterName (Just tuiOutputChannel)
      case result of
        Left err -> do
          systemMessage tuiOutputChannel $ "Erro ao iniciar servidor: " <> T.pack err
          return True
        Right serverState -> do
          -- Salva estado de rede
          NetworkState.setNetworkState (NetworkState.ServerState serverState)

          -- Atualiza contexto para modo multiplayer
          let updatedCtx =
                ctx
                  { GameContext.isMultiplayer = True,
                    GameContext.multiplayerSessionId = Just (T.pack $ "session-" ++ show NetworkServer.defaultPort)
                  }
          void $ GameContext.saveContext updatedCtx
          -- Atualiza o contexto no MVar também (saveContext só salva no arquivo)
          cache <- GameContext.getContextCache
          modifyMVar_ cache $ \_ -> return (Just updatedCtx)

          -- Obtém IP local
          maybeIP <- NetworkServer.getLocalIP
          let ipStr = fromMaybe "localhost" maybeIP

          -- Mostra informações na TUI
          systemMessage tuiOutputChannel $ "Servidor iniciado na porta " <> T.pack (show NetworkServer.defaultPort)
          systemMessage tuiOutputChannel $ "IP: " <> T.pack ipStr <> ":" <> T.pack (show NetworkServer.defaultPort)
          systemMessage tuiOutputChannel "Aguardando conexões..."

          return True

-- | Conecta a uma sessão multiplayer
connectSession :: TChan GameOutput -> T.Text -> IO Bool
connectSession tuiOutputChannel input = do
  maybeCtx <- GameContext.getCurrentContext
  case maybeCtx of
    Nothing -> do
      systemMessage tuiOutputChannel "Erro: Nenhum personagem carregado. Use :load para carregar um personagem."
      return True
    Just ctx -> do
      -- Parse do input (formato: "ip:port" ou "ip port")
      let parts = T.splitOn ":" (T.strip input)
      if length parts == 2
        then do
          let host = T.unpack (head parts)
          let portStr = parts !! 1
          case TR.decimal portStr of
            Right (port, _) -> do
              let playerName = GameContext.getCharacterName ctx
              let characterName = GameContext.name (GameContext.mainCharacter ctx)

              -- Conecta ao servidor
              result <- NetworkClient.connectToServer host (fromIntegral port) playerName characterName
              case result of
                Left err -> do
                  systemMessage tuiOutputChannel $ "Erro ao conectar: " <> T.pack err
                  return True
                Right clientState -> do
                  -- Salva estado de rede
                  NetworkState.setNetworkState (NetworkState.ClientState clientState)

                  -- Atualiza contexto para modo multiplayer
                  let updatedCtx =
                        ctx
                          { GameContext.isMultiplayer = True,
                            GameContext.multiplayerSessionId = Just (T.pack $ host ++ ":" ++ show port)
                          }
                  
                  -- CORRIGIDO: Atualiza o cache em memória primeiro, depois salva no arquivo
                  cache <- GameContext.getContextCache
                  modifyMVar_ cache $ \_ -> return (Just updatedCtx)
                  void $ GameContext.saveContext updatedCtx

                  -- Inicia loop de recepção
                  NetworkClient.startReceiveLoop clientState tuiOutputChannel

                  systemMessage tuiOutputChannel "Conectado ao servidor!"
                  return True
            Left _ -> do
              systemMessage tuiOutputChannel "Formato inválido. Use: :connect <ip:port>"
              return True
        else do
          systemMessage tuiOutputChannel "Formato inválido. Use: :connect <ip:port>"
          return True

-- | Desconecta da sessão multiplayer
disconnectSession :: TChan GameOutput -> T.Text -> IO Bool
disconnectSession tuiOutputChannel _input = do
  networkState <- NetworkState.getNetworkState
  case networkState of
    NetworkState.ServerState serverState -> do
      NetworkServer.stopServer serverState
      NetworkState.clearNetworkState
      maybeCtx <- GameContext.getCurrentContext
      for_ maybeCtx $ \ctx -> do
        let updatedCtx =
              ctx
                { GameContext.isMultiplayer = False,
                  GameContext.multiplayerSessionId = Nothing
                }
        void $ GameContext.saveContext updatedCtx
      systemMessage tuiOutputChannel "Servidor encerrado."
    NetworkState.ClientState clientState -> do
      NetworkClient.disconnectFromServer clientState
      NetworkState.clearNetworkState
      maybeCtx <- GameContext.getCurrentContext
      for_ maybeCtx $ \ctx -> do
        let updatedCtx =
              ctx
                { GameContext.isMultiplayer = False,
                  GameContext.multiplayerSessionId = Nothing
                }
        void $ GameContext.saveContext updatedCtx
      systemMessage tuiOutputChannel "Desconectado do servidor."
    NetworkState.NoNetworkState -> do
      systemMessage tuiOutputChannel "Não há sessão multiplayer ativa."
  return True

-- | Cria um voto compartilhado
sharedVow :: TChan GameOutput -> T.Text -> IO Bool
sharedVow tuiOutputChannel input = withContext $ \ctx -> do
  -- Verifica se está em modo multiplayer
  if not (GameContext.isMultiplayer ctx)
    then do
      systemMessage tuiOutputChannel "Shared vows só podem ser criados em modo multiplayer."
      return True
    else do
      maybe handleParseError (processSharedVow ctx) (Parser.parseQuotedString input)
  where
    handleParseError = systemMessage tuiOutputChannel (T.pack (C.msgVowUsage C.moveMessages)) >> return True
    processSharedVow ctx (vowName, rest) = maybe (handleInvalidRank rest) (createSharedVow ctx vowName) (Parser.parseRank (T.strip rest))
    handleInvalidRank rest = do
      systemMessage tuiOutputChannel $ T.pack (C.errInvalidRank C.errorMessages) <> T.strip rest
      systemMessage tuiOutputChannel $ T.pack (C.msgVowUsage C.moveMessages)
      return True
    createSharedVow ctx vowName rank = do
      let track = Progress.newProgressTrack vowName Progress.Vow rank
      let ticks = Progress.getTicksForRank rank
      updatedCtx <- GameContext.addProgressTrack ctx track
      _ <- GameContext.saveContext updatedCtx
      let formattedMsg = T.pack $ C.formatVowCreated C.moveMessages vowName (T.unpack $ Parser.rankToText rank) ticks
      _ <- process tuiOutputChannel AddStoryLog formattedMsg
      
      -- Faz broadcast do shared vow
      syncSharedVowCreated tuiOutputChannel updatedCtx vowName track
      
      return True

-- | Aceita uma conexão pendente
-- Se não fornecer nome, aceita a primeira conexão pendente
acceptConnection :: TChan GameOutput -> T.Text -> IO Bool
acceptConnection tuiOutputChannel input = do
  networkState <- NetworkState.getNetworkState
  case networkState of
    NetworkState.ServerState serverState -> do
      let playerName = T.strip input
      maybeConn <- NetworkServer.approveConnection serverState playerName
      case maybeConn of
        Nothing -> do
          if T.null playerName
            then systemMessage tuiOutputChannel "Nenhuma conexão pendente para aceitar."
            else systemMessage tuiOutputChannel $ "Nenhuma conexão pendente encontrada para " <> playerName
        Just conn -> do
          systemMessage tuiOutputChannel $ "Conexão aceita: " <> NetworkTypes.connPlayerName conn <> " (" <> NetworkTypes.connCharacterName conn <> ")"
          -- receiveLoop já foi iniciado pelo approveConnection
          -- Lista de jogadores será atualizada automaticamente pelo acceptConnection
      return True
    _ -> do
      systemMessage tuiOutputChannel "Apenas o host pode aceitar conexões."
      return True

-- | Rejeita uma conexão pendente
-- Se não fornecer nome, rejeita a primeira conexão pendente
rejectConnection :: TChan GameOutput -> T.Text -> IO Bool
rejectConnection tuiOutputChannel input = do
  networkState <- NetworkState.getNetworkState
  case networkState of
    NetworkState.ServerState serverState -> do
      let playerName = T.strip input
      NetworkServer.rejectPendingConnection serverState playerName
      systemMessage tuiOutputChannel $ "Conexão rejeitada: " <> (if T.null playerName then "primeira pendente" else playerName)
      return True
    _ -> do
      systemMessage tuiOutputChannel "Apenas o host pode rejeitar conexões."
      return True

-- | Sincroniza um progress track em modo multiplayer
syncProgressTrack :: TChan GameOutput -> GameContext.Context -> T.Text -> Progress.ProgressTrack -> IO ()
syncProgressTrack tuiOutputChannel ctx trackName track = do
  -- Apenas sincroniza Combat e Journey
  when (Progress.trackType track == Progress.Combat || Progress.trackType track == Progress.Journey) $ do
    networkState <- NetworkState.getNetworkState
    let playerName = GameContext.getCharacterName ctx
    let trackData = TE.decodeUtf8 $ BL.toStrict (Aeson.encode track)  -- Serializa track para JSON string
    
    case networkState of
      NetworkState.ServerState serverState -> do
        let msg = NetworkProtocol.ProgressTrackSync playerName trackName trackData
        NetworkServer.broadcastMessage serverState msg Nothing
      NetworkState.ClientState clientState -> do
        let msg = NetworkProtocol.ProgressTrackSync playerName trackName trackData
        void $ NetworkClient.sendMessage clientState msg
      NetworkState.NoNetworkState -> return ()
