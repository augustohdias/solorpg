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
import Data.Foldable (for_, forM_, find)
import Data.Maybe (fromMaybe)

-- | Creates a new Action Service handle that orchestrates communication between
-- game subsystems and the TUI. This service acts as the central coordinator,
-- delegating operations to appropriate modules and managing message flow.
--
-- The service follows a recursive architecture where actions can trigger other
-- actions through 'Action.process', maintaining consistent message handling.
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
      Action.SwearVow -> swearVow aCtx actionHandle
      Action.MarkProgress -> markProgress aCtx actionHandle
      Action.RollProgress -> rollProgress aCtx actionHandle
      Action.ShowTracks -> showTracks aCtx
      Action.AbandonTrack -> abandonTrack aCtx actionHandle
      Action.Bond -> bondCommand aCtx actionHandle
      Action.Oracle -> oracleQuery aCtx actionHandle
      Action.Help -> helpCommand aCtx
      _ -> defaultAction aCtx

-- | Sends narrative messages to the TUI log channel.
-- Used exclusively for story events, dice rolls, oracle results, and move outcomes.
-- These messages become part of the character's session history.
logMessage :: ActionContext -> T.Text -> IO ()
logMessage ActionContext { tuiOutputChannel } msg =
  atomically $ writeTChan tuiOutputChannel (LogEntry msg NarrativeMessage)

-- | Sends system notifications to the TUI.
-- Used for error messages, status updates, help text, and other non-narrative communication.
-- These messages are not stored in the session history.
systemMessage :: ActionContext -> T.Text -> IO ()
systemMessage ActionContext { tuiOutputChannel } msg =
  atomically $ writeTChan tuiOutputChannel (LogEntry msg SystemMessage)

-- | Executes an action that requires a loaded character context.
-- Displays an error message and returns True if no character is loaded.
-- Otherwise executes the provided action with the current context.
withContext :: ActionContext -> (GameContext.Context -> IO Bool) -> IO Bool
withContext ActionContext { contextHandler } action = do
  maybeCtx <- GameContext.getCurrentContext contextHandler
  case maybeCtx of
    Nothing -> pure True
    Just ctx -> action ctx

-- | Conditionally executes an action with the current context if one exists.
-- Does nothing silently if no context is loaded (no error message).
-- Used for operations that should only run when a character is available.
whenContext :: ActionContext -> (GameContext.Context -> IO ()) -> IO ()
whenContext ActionContext { contextHandler } action = do
  maybeCtx <- GameContext.getCurrentContext contextHandler
  for_ maybeCtx action

-- | Default handler for unknown or unimplemented action types.
-- Always returns False to indicate the action was not processed.
defaultAction :: ActionContext -> T.Text -> IO Bool
defaultAction _ _ = return False

-- | Adds a narrative entry to the character's session log.
-- Ignores entries starting with ':' (commands) and only processes story text.
-- Automatically saves the updated context and displays the message.
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

-- | Executes dice rolls using dice specification strings.
-- Delegates log creation to Action.process following the recursive architecture.
-- Displays atmospheric message about fate being cast.
--
-- Example: rollDice ctx handle "2d6,1d10"
rollDice :: ActionContext -> Action.Handle -> T.Text -> IO Bool
rollDice ActionContext { diceHandler } actionH diceDescription = do
  rolls <- Dice.roll diceHandler diceDescription
  let msg = C.msgDiceRolled C.messages <> T.pack (show rolls)
  _ <- Action.process actionH Action.AddStoryLog msg
  return True

-- | Displays the current character's session logs.
-- Shows a header followed by all logged narrative entries in chronological order.
-- Requires an active character context to function.
showLogs :: ActionContext -> T.Text -> IO Bool
showLogs aCtx _ = withContext aCtx $ \ctx -> do
  systemMessage aCtx (C.msgLogsHeader C.messages)
  let logs = GameContext.getSessionLog (contextHandler aCtx) ctx
  mapM_ (logMessage aCtx) logs
  return True

-- | Terminates the current game session.
-- Sends a session end message and triggers TUI shutdown.
-- Returns False to indicate the action loop should stop.
exit :: ActionContext -> T.Text -> IO Bool
exit aCtx@ActionContext { tuiOutputChannel } _ = do
  systemMessage aCtx (C.msgSessionEnded C.messages)
  atomically $ writeTChan tuiOutputChannel GameEnd
  return False

-- | Creates a new character with specified name and attributes.
-- Input format: "CharacterName iron:3 edge:2 heart:2 shadow:1 wits:2"
-- Missing attributes default to configured values. Updates TUI with character data.
createCharacter :: ActionContext -> T.Text -> IO Bool
createCharacter aCtx@ActionContext { contextHandler, tuiOutputChannel } input = do
  let parts = T.words input
  case parts of
    [] -> systemMessage aCtx (C.msgCharacterNameRequired C.messages) >> return True
    (charName:attrParts) -> do
      let attrs = Parser.parseAttributes attrParts
      result <- GameContext.createContext contextHandler charName attrs
      either handleError handleSuccess result
  where
    handleError err = systemMessage aCtx (C.msgErrorCreating C.messages <> T.pack (show err)) >> return True

    handleSuccess ctx = do
      let msg = C.msgCharacterCreated C.messages <> GameContext.getCharacterName ctx
      systemMessage aCtx msg
      _ <- GameContext.saveContext contextHandler ctx
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter ctx))
      return True

-- | Loads an existing character from persistent storage.
-- Restores character data, displays previous session logs, and updates TUI.
-- Shows error message if character file cannot be found or loaded.
loadCharacter :: ActionContext -> T.Text -> IO Bool
loadCharacter aCtx@ActionContext { contextHandler, tuiOutputChannel } charName
  | T.null charName = systemMessage aCtx (C.msgCharacterNameRequired C.messages) >> return True
  | otherwise = do
      result <- GameContext.loadContext contextHandler charName
      either handleError handleSuccess result
  where
    handleError err = systemMessage aCtx (C.msgErrorLoading C.messages <> T.pack (show err)) >> return True

    handleSuccess ctx = do
      let msg = C.msgCharacterLoaded C.messages <> GameContext.getCharacterName ctx
      systemMessage aCtx msg
      let sessionLogs = reverse $ GameContext.getSessionLog contextHandler ctx
      mapM_ (logMessage aCtx) sessionLogs
      _ <- GameContext.saveContext contextHandler ctx
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter ctx))
      return True

-- | Displays current character information in the TUI.
-- Updates the character display panel without adding log entries.
-- Requires an active character context to function.
showCharacter :: ActionContext -> T.Text -> IO Bool
showCharacter aCtx@ActionContext { tuiOutputChannel } _ = withContext aCtx $ \ctx -> do
  let char = GameContext.mainCharacter ctx
  atomically $ writeTChan tuiOutputChannel (CharacterUpdate char)
  return True

-- | Updates a character attribute to an absolute value.
-- Input format: "attribute:value" (e.g., "iron:3", "edge:2")
-- Updates character sheet display and saves context automatically.
updateAttribute :: ActionContext -> T.Text -> IO Bool
updateAttribute aCtx@ActionContext { contextHandler, tuiOutputChannel } input = withContext aCtx $ \ctx -> do
  let oldAttrs = GameContext.attributes (GameContext.mainCharacter ctx)
  maybe handleParseError (handleUpdate ctx) (Parser.parseAttributeUpdate input oldAttrs)
  where
    handleParseError = systemMessage aCtx (C.msgInvalidAttributeFormat C.messages) >> return True

    handleUpdate ctx newAttrs = do
      updatedCtx <- GameContext.updateAttributes contextHandler ctx newAttrs
      _ <- GameContext.saveContext contextHandler updatedCtx
      systemMessage aCtx (C.msgAttributeUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Updates a character resource to an absolute value.
-- Input format: "resource:value" (e.g., "health:4", "momentum:2")
-- Updates character sheet display and saves context automatically.
updateResource :: ActionContext -> T.Text -> IO Bool
updateResource aCtx@ActionContext { contextHandler, tuiOutputChannel } input = withContext aCtx $ \ctx -> do
  let oldRes = GameContext.resources (GameContext.mainCharacter ctx)
  maybe handleParseError (handleUpdate ctx) (Parser.parseResourceUpdate input oldRes)
  where
    handleParseError = systemMessage aCtx (C.msgInvalidResourceFormat C.messages) >> return True

    handleUpdate ctx newRes = do
      updatedCtx <- GameContext.updateResources contextHandler ctx newRes
      _ <- GameContext.saveContext contextHandler updatedCtx
      systemMessage aCtx (C.msgResourceUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Modifies a character attribute by a delta value.
-- Input format: "attribute:delta" (e.g., "iron:-1", "edge:+2")
-- Supports both positive and negative modifications with automatic bounds checking.
addAttribute :: ActionContext -> T.Text -> IO Bool
addAttribute aCtx@ActionContext { contextHandler, tuiOutputChannel } input = withContext aCtx $ \ctx -> do
  let oldAttrs = GameContext.attributes (GameContext.mainCharacter ctx)
  maybe handleParseError (handleUpdate ctx) (Parser.parseAttributeAdd input oldAttrs)
  where
    handleParseError = systemMessage aCtx "Formato inválido. Use: atributo:delta (ex: iron:-1, edge:+2)" >> return True

    handleUpdate ctx newAttrs = do
      updatedCtx <- GameContext.updateAttributes contextHandler ctx newAttrs
      _ <- GameContext.saveContext contextHandler updatedCtx
      systemMessage aCtx (C.msgAttributeUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Modifies a character resource by a delta value.
-- Input format: "resource:delta" (e.g., "health:-1", "momentum:+2")
-- Applies resource-specific bounds (health/spirit/supply: 0-5, momentum: unbounded).
addResource :: ActionContext -> T.Text -> IO Bool
addResource aCtx@ActionContext { contextHandler, tuiOutputChannel } input = withContext aCtx $ \ctx -> do
  let oldRes = GameContext.resources (GameContext.mainCharacter ctx)
  maybe handleParseError (handleUpdate ctx) (Parser.parseResourceAdd input oldRes)
  where
    handleParseError = systemMessage aCtx "Formato inválido. Use: recurso:delta (ex: health:-1, momentum:+2)" >> return True

    handleUpdate ctx newRes = do
      updatedCtx <- GameContext.updateResources contextHandler ctx newRes
      _ <- GameContext.saveContext contextHandler updatedCtx
      systemMessage aCtx (C.msgResourceUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Executes an Ironsworn challenge roll (1d6 + 2d10).
-- Delegates dice rolling to DiceService and result formatting to DiceContract.
-- Automatically evaluates Strong Hit/Weak Hit/Miss and handles matches.
challenge :: ActionContext -> Action.Handle -> T.Text -> IO Bool
challenge aCtx@ActionContext { diceHandler } actionH _ = do
  result <- Dice.challengeRoll diceHandler
  either handleError handleSuccess result
  where
    handleError err = systemMessage aCtx err >> return True

    handleSuccess challengeResult = do
      let formattedMsg = Dice.formatChallengeResult challengeResult
            (C.formatActionRoll C.characterDisplay)
            (C.challengeStrongHit C.challengeInterpretation)
            (C.challengeWeakHit C.challengeInterpretation)
            (C.challengeMiss C.challengeInterpretation)
            (C.challengeMatch C.challengeInterpretation)
      _ <- Action.process actionH Action.AddStoryLog formattedMsg
      return True

-- | Executes an Ironsworn move with optional stat modifier.
-- Parses move name and parameters, delegates execution to MoveService,
-- then processes all resulting consequences through the action system.
moveAction :: ActionContext -> Action.Handle -> T.Text -> IO Bool
moveAction aCtx@ActionContext { moveHandler } actionH input = withContext aCtx $ \ctx -> do
  let parts = T.words input
  case parts of
    [] -> systemMessage aCtx (T.pack (C.msgMoveUsage C.helpMessages)) >> return True
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
          consequences <- Move.executeMove moveHandler moveType maybeStat attrs resources
          processConsequences aCtx actionH consequences ctx >> return True

-- | Parses move command parameters into optional stat and remaining text.
-- Attempts to identify the first parameter as a stat (iron, edge, etc.).
-- Returns (Maybe Stat, remaining parameters as text).
parseMoveParams :: Move.Handle -> [T.Text] -> (Maybe Move.Stat, T.Text)
parseMoveParams _ [] = (Nothing, "")
parseMoveParams moveHandler params@(first:rest) = (stat, text) where
  stat = Move.parseStat moveHandler first
  text = maybe (T.unwords params) (const (T.unwords rest)) stat

-- | Processes a list of consequences from moves or oracle results.
-- Applies each consequence sequentially, handling resource changes,
-- triggered actions, player choices, and narrative elements.
processConsequences :: ActionContext -> Action.Handle -> [Consequence] -> GameContext.Context -> IO ()
processConsequences aCtx actionH consequences ctx = do
  mapM_ (processConsequence aCtx actionH ctx) consequences

-- | Processes a single consequence using the recursive action architecture.
-- Delegates resource changes and action triggers to Action.process for consistency.
-- Handles narrative text, resource modifications, move chaining, and player choices.
processConsequence :: ActionContext -> Action.Handle -> GameContext.Context -> Consequence -> IO ()
processConsequence aCtx@ActionContext { contextHandler } actionH ctx cons = case cons of
  Narrative text ->
    void $ Action.process actionH Action.AddStoryLog text

  LoseHealth amount ->
    applyResourceDelta aCtx actionH "health" (-amount)

  LoseSpirit amount ->
    applyResourceDelta aCtx actionH "spirit" (-amount)

  LoseSupply amount ->
    applyResourceDelta aCtx actionH "supply" (-amount)

  LoseMomentum amount ->
    applyResourceDelta aCtx actionH "momentum" (-amount)

  GainHealth amount ->
    applyResourceDelta aCtx actionH "health" amount

  GainSpirit amount ->
    applyResourceDelta aCtx actionH "spirit" amount

  GainSupply amount ->
    applyResourceDelta aCtx actionH "supply" amount

  GainMomentum amount ->
    applyResourceDelta aCtx actionH "momentum" amount

  TriggerMove nextMoveType -> do
    logMessage aCtx $ "\n>>> Executando " <> Consequence.moveTypeToText nextMoveType <> " automaticamente..."
    let nextMoveText = T.toLower (Consequence.moveTypeToText nextMoveType)
    _ <- Action.process actionH Action.Move nextMoveText
    return ()

  TriggerOracle oracleName -> do
    logMessage aCtx $ "\n[*] Consultando oráculo " <> oracleName <> " automaticamente..."
    _ <- Action.process actionH Action.Oracle oracleName
    return ()

  PlayerChoice choices -> do
    maybeChoice <- Move.showChoices (moveHandler aCtx) choices
    case maybeChoice of
      Just choice -> do
        logMessage aCtx $ "\nVocê escolheu: " <> choiceDescription choice
        processConsequences aCtx actionH (choiceConsequences choice) ctx
      Nothing ->
        systemMessage aCtx "Nenhuma escolha válida."

  AddBonus bonus -> do
    updatedCtx <- GameContext.addBonus contextHandler ctx bonus
    _ <- GameContext.saveContext contextHandler updatedCtx
    systemMessage aCtx $ "Bônus adicionado: " <> GameContext.bonusDescription bonus <> " (+" <> T.pack (show (GameContext.bonusValue bonus)) <> ")"

  MarkBondProgress -> do
    markBondProgressTrack aCtx actionH ctx

-- | Marks progress on the bonds track when Forge a Bond succeeds.
-- Creates the bonds track if it doesn't exist (rank doesn't matter for bonds track).
-- Marks 1 tick (¼ box) as per Ironsworn rules.
markBondProgressTrack :: ActionContext -> Action.Handle -> GameContext.Context -> IO ()
markBondProgressTrack aCtx@ActionContext { contextHandler, progressHandler } _actionH _ctx = do
  -- Always get the current context from MVar to ensure we have the latest state
  maybeCurrentCtx <- GameContext.getCurrentContext contextHandler
  case maybeCurrentCtx of
    Nothing -> return ()  -- No context loaded, can't mark progress
    Just currentCtx -> do
      let bondTrackName = "Bonds"
      let maybeTrack = GameContext.getProgressTrack contextHandler currentCtx bondTrackName
      
      (track, wasCreated) <- case maybeTrack of
        Just existingTrack -> return (existingTrack, False)
        Nothing -> do
          let newTrack = Progress.newProgressTrack bondTrackName Progress.Bond Progress.Troublesome
          return (newTrack, True)
      
      -- Mark 1 tick (as per Ironsworn rules: 1 tick per Forge a Bond Strong Hit)
      updatedTrack <- Progress.markProgressTicks progressHandler track 1
      
      -- Add track if it was just created, otherwise update it
      updatedCtx <- if wasCreated
        then GameContext.addProgressTrack contextHandler currentCtx updatedTrack
        else GameContext.updateProgressTrack contextHandler currentCtx bondTrackName updatedTrack
      
      _ <- GameContext.saveContext contextHandler updatedCtx
      
      let boxes = Progress.getProgressScore updatedTrack
      let ticks = Progress.trackTicks updatedTrack
      logMessage aCtx $ "[+] Bond progress: " <> T.pack (show boxes) <> "/10 boxes (" <> T.pack (show ticks) <> "/40 ticks)"

-- | Applies a delta change to character resources through Action.process.
-- Formats the delta as a command string and delegates to AddResource action.
-- Maintains architectural consistency by avoiding direct resource manipulation.
applyResourceDelta :: ActionContext -> Action.Handle -> T.Text -> Int -> IO ()
applyResourceDelta _aCtx actionH resourceName delta = do
  let command = resourceName <> ":" <> deltaText
  void (Action.process actionH Action.AddResource command) where
  deltaText
    | delta >= 0 ="+" <> T.pack (show delta)
    | otherwise = T.pack (show delta)

-- | Queries oracle systems with optional specific values.
-- Empty input lists available oracles. Named input triggers random roll.
-- Input with value performs direct lookup. Delegates all operations to OracleService.
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
        then systemMessage aCtx "Nenhum oráculo carregado."
        else do
          systemMessage aCtx "=== Oráculos Disponíveis ==="
          mapM_ (systemMessage aCtx . ("• " <>)) oracles
      return True

-- | Executes a random oracle roll and processes the result.
-- Formats the oracle response and delegates log creation to Action.process.
-- Automatically executes any structured consequences from the oracle result.
rollOracleRandomly :: ActionContext -> Action.Handle -> T.Text -> IO Bool
rollOracleRandomly aCtx@ActionContext { oracleHandler } actionH oracleName = do
  result <- Oracle.rollOracle oracleHandler oracleName
  case result of
    Left err -> do
      systemMessage aCtx $ "Erro no oráculo: " <> T.pack (show err)
      return True
    Right oracleResult -> do
      let formattedResult = T.pack $
            "[*] Oráculo: " ++ T.unpack (Oracle.resultOracle oracleResult) ++
            " (Rolou " ++ show (Oracle.resultRoll oracleResult) ++ "):\n" ++
            "-> " ++ T.unpack (Oracle.resultText oracleResult)

      _ <- Action.process actionH Action.AddStoryLog formattedResult
      executeOracleConsequences aCtx actionH oracleResult
      return True

-- | Executes an oracle query with a specific index value.
-- Parses the numeric value and performs direct oracle lookup.
-- Formats result and delegates log creation to Action.process.
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
                "[*] Oráculo: " ++ T.unpack (Oracle.resultOracle oracleResult) ++
                " (Índice " ++ show (Oracle.resultRoll oracleResult) ++ "):\n" ++
                "-> " ++ T.unpack (Oracle.resultText oracleResult)

          _ <- Action.process actionH Action.AddStoryLog formattedResult
          executeOracleConsequences aCtx actionH oracleResult
          return True
    Left _ -> do
      systemMessage aCtx "Valor inválido para consulta direta."
      return True

-- | Executes structured consequences from oracle results.
-- Processes any consequence chains that may result from oracle queries,
-- maintaining consistency with the move consequence system.
executeOracleConsequences :: ActionContext -> Action.Handle -> Oracle.OracleResult -> IO ()
executeOracleConsequences aCtx@ActionContext { contextHandler } actionH result = do
  let structuredConsequences = Oracle.resultConsequences result
  unless (null structuredConsequences) $ do
    maybeCtx <- GameContext.getCurrentContext contextHandler
    for_ maybeCtx $ \ctx ->
      processConsequences aCtx actionH structuredConsequences ctx

-- | Displays help information for game commands and topics.
-- Shows general help when no topic specified, or specific topic help.
-- All help text is sent as system messages, not narrative entries.
helpCommand :: ActionContext -> T.Text -> IO Bool
helpCommand aCtx@ActionContext { helpHandler } input = do
  let topic = T.strip input
  if T.null topic
    then do
      helpText <- Help.showHelp helpHandler
      systemMessage aCtx (T.pack helpText)
    else case Help.parseTopic helpHandler topic of
      Just t -> do
        helpText <- Help.showTopicHelp helpHandler t
        systemMessage aCtx (T.pack helpText)
      Nothing -> do
        systemMessage aCtx $ "Tópico desconhecido: " <> topic
        systemMessage aCtx "Tópicos disponíveis: moves, progress, oracle, chaining, character"
  return True

-- | Creates a new vow (Ironsworn progress track).
-- Input format: "Vow Name" rank (e.g., "Avenge my father" dangerous)
-- Delegates log creation to Action.process following recursive architecture.
swearVow :: ActionContext -> Action.Handle -> T.Text -> IO Bool
swearVow aCtx@ActionContext { contextHandler } actionH input = withContext aCtx $ \ctx -> do
  maybe handleParseError (processVow ctx) (Parser.parseQuotedString input)
  where
    handleParseError = systemMessage aCtx (T.pack (C.msgVowUsage C.moveMessages)) >> return True
    processVow ctx (vowName, rest) = maybe (handleInvalidRank rest) (createVow ctx vowName) (Parser.parseRank (T.strip rest))
    handleInvalidRank rest = do
      systemMessage aCtx $ T.pack (C.errInvalidRank C.errorMessages) <> T.strip rest
      systemMessage aCtx $ T.pack (C.msgVowUsage C.moveMessages)
      return True
    createVow ctx vowName rank = do
      let track = Progress.newProgressTrack vowName Progress.Vow rank
      let ticks = Progress.getTicksForRank rank
      updatedCtx <- GameContext.addProgressTrack contextHandler ctx track
      _ <- GameContext.saveContext contextHandler updatedCtx
      let formattedMsg = T.pack $ C.formatVowCreated C.moveMessages vowName (T.unpack $ Parser.rankToText rank) ticks
      _ <- Action.process actionH Action.AddStoryLog formattedMsg
      return True

-- | Marks progress on an existing progress track.
-- Adds progress ticks based on the track's difficulty rank.
-- Creates narrative log entry showing current progress status.
markProgress :: ActionContext -> Action.Handle -> T.Text -> IO Bool
markProgress aCtx@ActionContext { contextHandler, progressHandler } actionH input = withContext aCtx $ \ctx -> do
  let trackName = T.strip input
  case nonEmpty trackName of
    Nothing -> systemMessage aCtx (T.pack (C.msgProgressUsage C.moveMessages)) >> return True
    Just name -> handleTrackName ctx name
  where
    handleTrackName ctx name = case GameContext.getProgressTrack contextHandler ctx name of
      Nothing -> systemMessage aCtx (C.msgTrackNotFound C.moveMessages <> name) >> return True
      Just track -> processTrack ctx name track

    processTrack ctx name track
      | Progress.trackCompleted track = systemMessage aCtx "Track já está completo!" >> return True
      | otherwise = do
          updatedTrack <- Progress.markProgress progressHandler track
          updatedCtx <- GameContext.updateProgressTrack contextHandler ctx name updatedTrack
          _ <- GameContext.saveContext contextHandler updatedCtx
          let boxes = Progress.getProgressScore updatedTrack
          let ticks = Progress.trackTicks updatedTrack
          let msg = "[+] Progresso marcado: " <> name <>
                   " (" <> T.pack (show boxes) <> "/10 boxes, " <>
                   T.pack (show ticks) <> "/40 ticks)"
          _ <- Action.process actionH Action.AddStoryLog msg
          return True

    nonEmpty s = if T.null s then Nothing else Just s

-- | Executes a progress roll to attempt completing a progress track.
-- Performs 2d10 vs progress score, handles completion, experience gain,
-- and track state changes. Delegates formatting to ProgressService.
rollProgress :: ActionContext -> Action.Handle -> T.Text -> IO Bool
rollProgress aCtx@ActionContext { contextHandler, progressHandler } actionH input = withContext aCtx $ \ctx -> do
  let trackName = T.strip input
  case nonEmpty trackName of
    Nothing -> systemMessage aCtx "Uso: :fulfill \"<nome do track>\"" >> return True
    Just name -> handleTrackName ctx name
  where
    handleTrackName ctx name = case GameContext.getProgressTrack contextHandler ctx name of
      Nothing -> systemMessage aCtx (C.msgTrackNotFound C.moveMessages <> name) >> return True
      Just track -> processTrack ctx name track

    processTrack ctx name track
      | Progress.trackCompleted track = systemMessage aCtx "Track já está completo!" >> return True
      | otherwise = do
          executionResult <- Progress.executeProgressRoll progressHandler track
          _ <- Action.process actionH Action.AddStoryLog (Progress.executionNarrativeMessage executionResult)
          updatedCtx <- GameContext.updateProgressTrack contextHandler ctx name (Progress.executionUpdatedTrack executionResult)
          finalCtx <- applyExperienceGain updatedCtx (Progress.executionExperienceGained executionResult)
          _ <- GameContext.saveContext contextHandler finalCtx
          forM_ (Progress.executionSystemMessage executionResult) (systemMessage aCtx)
          return True

    applyExperienceGain ctx expGained
      | expGained > 0 = do
          let command = "experience:+" <> T.pack (show expGained)
          _ <- Action.process actionH Action.AddResource command
          maybeCtx <- GameContext.getCurrentContext contextHandler
          return $ fromMaybe ctx maybeCtx
      | otherwise = return ctx

    nonEmpty s = if T.null s then Nothing else Just s

-- | Displays all active progress tracks.
-- Shows track names, types, ranks, and completion status.
-- Formats output using Parser utilities for consistent display.
showTracks :: ActionContext -> T.Text -> IO Bool
showTracks aCtx _ = withContext aCtx $ \ctx -> do
  let tracks = GameContext.progressTracks ctx
  if null tracks
    then do
      systemMessage aCtx $ T.pack (C.msgNoTracksActive C.moveMessages)
    else do
      systemMessage aCtx $ T.pack (C.msgTracksHeader C.moveMessages)
      mapM_ (systemMessage aCtx . Parser.formatProgressTrack) tracks
  return True

-- | Abandons and removes a progress track from the character context.
-- Equivalent to "Forsake Your Vow" in Ironsworn terminology.
-- Creates narrative log entry about the abandoned track.
abandonTrack :: ActionContext -> Action.Handle -> T.Text -> IO Bool
abandonTrack aCtx@ActionContext { contextHandler } actionH input = withContext aCtx $ \ctx -> do
  let trackName = T.strip input
  case nonEmpty trackName of
    Nothing -> systemMessage aCtx "Uso: :abandon \"<nome do track>\"" >> return True
    Just name -> handleTrackName ctx name
  where
    handleTrackName ctx name = case GameContext.getProgressTrack contextHandler ctx name of
      Nothing -> systemMessage aCtx (C.msgTrackNotFound C.moveMessages <> name) >> return True
      Just _track -> removeTrack ctx name

    removeTrack ctx name = do
      updatedCtx <- GameContext.removeProgressTrack contextHandler ctx name
      _ <- GameContext.saveContext contextHandler updatedCtx
      let msg = C.msgTrackRemoved C.moveMessages <> name
      _ <- Action.process actionH Action.AddStoryLog msg
      return True

    nonEmpty s = if T.null s then Nothing else Just s

-- | Manages character bonds (relationships with people, communities, and places).
-- Supports listing, adding, removing, and updating notes through subcommands.
-- All bond operations update the character context and provide system feedback.
-- Adding a bond also marks progress on the bonds track (as per Ironsworn rules).
bondCommand :: ActionContext -> Action.Handle -> T.Text -> IO Bool
bondCommand aCtx@ActionContext { contextHandler } actionH input = withContext aCtx $ \ctx -> do
  let parts = T.words (T.strip input)
  case parts of
    [] -> listBonds aCtx ctx
    ("add":nameParts) -> addBond aCtx actionH ctx (T.unwords nameParts) GameContext.PersonBond
    ("addplace":nameParts) -> addBond aCtx actionH ctx (T.unwords nameParts) GameContext.PlaceBond
    ("addcomm":nameParts) -> addBond aCtx actionH ctx (T.unwords nameParts) GameContext.CommunityBond
    ("remove":nameParts) -> removeBond aCtx ctx (T.unwords nameParts)
    ("note":rest) -> updateBondNotes aCtx ctx (T.unwords rest)
    _ -> do
      systemMessage aCtx "Uso: :bond [add|addplace|addcomm|remove|note] \"<nome>\" [\"<nota>\"] ou :bond para listar"
      return True
  where
    listBonds aCtx' _ctx = do
      -- Always get current context from MVar to ensure we have the latest bonds
      maybeCurrentCtx <- GameContext.getCurrentContext contextHandler
      case maybeCurrentCtx of
        Nothing -> do
          systemMessage aCtx' "Erro: contexto não carregado."
          return True
        Just currentCtx -> do
          let bonds = GameContext.listBonds contextHandler currentCtx
          if null bonds
            then systemMessage aCtx' "Nenhum bond registrado."
            else mapM_ (showBond aCtx') bonds
          return True

    showBond aCtx' bond = do
      let bondType = case GameContext.bondType bond of
            GameContext.PersonBond -> "Pessoa"
            GameContext.CommunityBond -> "Comunidade"
            GameContext.PlaceBond -> "Lugar"
      systemMessage aCtx' $ "• " <> GameContext.bondName bond <> " (" <> T.pack bondType <> ")"
      unless (T.null (GameContext.bondNotes bond)) $
        systemMessage aCtx' $ "  " <> GameContext.bondNotes bond

    addBond aCtx' actionH' _ctx bondName bondType
      | T.null bondName = do
          let typeHint = case bondType of
                GameContext.PersonBond -> "Uso: :bond add \"<nome>\""
                GameContext.PlaceBond -> "Uso: :bond addplace \"<nome>\""
                GameContext.CommunityBond -> "Uso: :bond addcomm \"<nome>\""
          systemMessage aCtx' typeHint
          return True
      | otherwise = do
          -- Always get current context from MVar to ensure we have the latest state
          maybeCurrentCtx <- GameContext.getCurrentContext contextHandler
          case maybeCurrentCtx of
            Nothing -> do
              systemMessage aCtx' "Erro: contexto não carregado."
              return True
            Just currentCtx -> do
              let bond = GameContext.Bond { GameContext.bondName = bondName
                                          , GameContext.bondType = bondType
                                          , GameContext.bondNotes = ""
                                          }
              updatedCtx <- GameContext.addBond contextHandler currentCtx bond
              _ <- GameContext.saveContext contextHandler updatedCtx
              -- Mark progress on bonds track (as per Ironsworn rules)
              markBondProgressTrack aCtx' actionH' updatedCtx
              let typeMsg = case bondType of
                    GameContext.PersonBond -> "Bond adicionado"
                    GameContext.PlaceBond -> "Bond com lugar adicionado"
                    GameContext.CommunityBond -> "Bond com comunidade adicionado"
              systemMessage aCtx' $ "[+] " <> T.pack typeMsg <> ": " <> bondName
              return True

    removeBond aCtx' _ctx bondName
      | T.null bondName = do
          systemMessage aCtx' "Uso: :bond remove \"<nome>\""
          return True
      | otherwise = do
          -- Always get current context from MVar to ensure we have the latest bonds
          maybeCurrentCtx <- GameContext.getCurrentContext contextHandler
          case maybeCurrentCtx of
            Nothing -> do
              systemMessage aCtx' "Erro: contexto não carregado."
              return True
            Just currentCtx -> do
              updatedCtx <- GameContext.removeBond contextHandler currentCtx bondName
              _ <- GameContext.saveContext contextHandler updatedCtx
              systemMessage aCtx' $ "[-] Bond removido: " <> bondName
              return True

    updateBondNotes aCtx' _ctx noteInput
      | T.null noteInput = do
          systemMessage aCtx' "Uso: :bond note \"<nome>\" \"<nota>\""
          return True
      | otherwise = case parseTwoQuotedStrings noteInput of
          Nothing -> do
            systemMessage aCtx' "Uso: :bond note \"<nome>\" \"<nota>\""
            return True
          Just (bondName, newNote) -> do
            -- Always get current context from MVar to ensure we have the latest bonds
            maybeCurrentCtx <- GameContext.getCurrentContext contextHandler
            case maybeCurrentCtx of
              Nothing -> do
                systemMessage aCtx' "Erro: contexto não carregado."
                return True
              Just currentCtx -> do
                let bonds = GameContext.listBonds contextHandler currentCtx
                case findBondByName bonds bondName of
                  Nothing -> do
                    systemMessage aCtx' $ "Bond não encontrado: " <> bondName
                    return True
                  Just existingBond -> do
                    let currentNotes = GameContext.bondNotes existingBond
                    let updatedNotes = if T.null currentNotes
                                       then newNote
                                       else currentNotes <> "\n" <> newNote
                    let updatedBond = existingBond { GameContext.bondNotes = updatedNotes }
                    updatedCtx <- GameContext.updateBond contextHandler currentCtx bondName updatedBond
                    _ <- GameContext.saveContext contextHandler updatedCtx
                    systemMessage aCtx' $ "[+] Nota adicionada a " <> bondName <> ": " <> newNote
                    return True

    parseTwoQuotedStrings :: T.Text -> Maybe (T.Text, T.Text)
    parseTwoQuotedStrings textInput = do
      (first, rest) <- Parser.parseQuotedString textInput
      (second, _) <- Parser.parseQuotedString (T.strip rest)
      return (first, second)

    findBondByName bonds name = find (\b -> GameContext.bondName b == name) bonds