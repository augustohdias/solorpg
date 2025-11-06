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
import qualified System.Constants as C
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import System.Tui.Comm (GameOutput(..), MessageType(..))
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Monad (void)
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
      Action.Challenge -> challenge aCtx
      Action.Move -> moveAction aCtx
      Action.SwearVow -> swearVow aCtx
      Action.MarkProgress -> markProgress aCtx
      Action.RollProgress -> rollProgress aCtx
      Action.ShowTracks -> showTracks aCtx
      Action.AbandonTrack -> abandonTrack aCtx
      Action.Oracle -> oracleQuery aCtx
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
  -- Ignora entradas que começam com : (são comandos, não narrativa)
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
  -- Delega a adição do log para o handle de ação
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

      -- Parse dos atributos (valores padrão se não fornecidos)
      let attrs = parseAttributes attrParts

      result <- GameContext.createContext contextHandler charName attrs
      case result of
        Left err -> do
          systemMessage aCtx (C.msgErrorCreating C.messages <> T.pack (show err))
          return True
        Right ctx -> do
          let msg = C.msgCharacterCreated C.messages <> GameContext.getCharacterName ctx
          systemMessage aCtx msg

          -- Não adiciona ao log (apenas mensagem de sistema)
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

          -- Load and display session logs (reverse order - newest first)
          let sessionLogs = reverse $ GameContext.getSessionLog contextHandler ctx
          mapM_ (logMessage aCtx) sessionLogs

          -- Não adiciona ao log (apenas mensagem de sistema)
          _ <- GameContext.saveContext contextHandler ctx
          atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter ctx))
          return True

-- | Mostra informações do personagem atual
showCharacter :: ActionContext -> T.Text -> IO Bool
showCharacter aCtx@ActionContext { tuiOutputChannel } _ = withContext aCtx $ \ctx -> do
  let char = GameContext.mainCharacter ctx
  atomically $ writeTChan tuiOutputChannel (CharacterUpdate char)
  -- Don't log message to avoid spam from periodic updates
  return True

-- | Atualiza atributo (define valor absoluto)
updateAttribute :: ActionContext -> T.Text -> IO Bool
updateAttribute aCtx@ActionContext { contextHandler, tuiOutputChannel } input = withContext aCtx $ \ctx -> do
  let oldAttrs = GameContext.attributes (GameContext.mainCharacter ctx)
  case parseAttributeUpdate input oldAttrs of
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
  case parseResourceUpdate input oldRes of
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
  case parseAttributeAdd input oldAttrs of
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
  case parseResourceAdd input oldRes of
    Nothing -> do
      systemMessage aCtx "Formato inválido. Use: recurso:delta (ex: health:-1, momentum:+2)"
      return True
    Just newRes -> do
      updatedCtx <- GameContext.updateResources contextHandler ctx newRes
      _ <- GameContext.saveContext contextHandler updatedCtx
      systemMessage aCtx (C.msgResourceUpdated C.messages)
      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter updatedCtx))
      return True

-- | Rola 1d6+2d10 e avalia resultado (challenge roll)

-- Mover para DiceService
challenge :: ActionContext -> T.Text -> IO Bool
challenge aCtx@ActionContext { diceHandler } _ = do
  -- Rola 1d6,2d10 automaticamente e avalia
  rolls <- Dice.roll diceHandler "1d6,2d10"
  case rolls of
    [(_, actionDie), (_, ch1), (_, ch2)] -> do
      let result = evaluateActionRoll actionDie ch1 ch2
      let resultMsg = showActionRollResult result
      let interpretation = case result of
            Dice.StrongHit -> C.challengeStrongHit C.challengeInterpretation
            Dice.WeakHit -> C.challengeWeakHit C.challengeInterpretation
            Dice.Miss -> C.challengeMiss C.challengeInterpretation
            Dice.InvalidRoll -> ""
      let matchMsg = if ch1 == ch2 then C.challengeMatch C.challengeInterpretation else ""

      let formattedMsg = T.pack $ C.formatActionRoll C.characterDisplay actionDie ch1 ch2 resultMsg
                      ++ interpretation ++ matchMsg
      logMessage aCtx formattedMsg

      return True
    _ -> do
      logMessage aCtx "Erro ao rolar dados para challenge"
      return True

moveAction, swearVow, markProgress, rollProgress, showTracks, abandonTrack, oracleQuery, helpCommand, bondCommand :: ActionContext -> T.Text -> IO Bool
moveAction _ _ = return True  -- TODO: Implement
swearVow _ _ = return True    -- TODO: Implement
markProgress _ _ = return True -- TODO: Implement
rollProgress _ _ = return True -- TODO: Implement
showTracks _ _ = return True   -- TODO: Implement
abandonTrack _ _ = return True -- TODO: Implement
oracleQuery _ _ = return True  -- TODO: Implement
helpCommand _ _ = return True  -- TODO: Implement
bondCommand _ _ = return True  -- TODO: Implement

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
evaluateActionRoll :: Int -> Int -> Int -> Dice.RollResult
evaluateActionRoll actionDie challenge1 challenge2
  | actionDie > challenge1 && actionDie > challenge2 = Dice.StrongHit
  | actionDie > challenge1 || actionDie > challenge2 = Dice.WeakHit
  | otherwise = Dice.Miss

-- | Converte ActionRollResult para string legível
showActionRollResult :: Dice.RollResult -> String
showActionRollResult Dice.StrongHit = "STRONG HIT"
showActionRollResult Dice.WeakHit = "WEAK HIT"
showActionRollResult Dice.Miss = "MISS"
showActionRollResult Dice.InvalidRoll = "INVALID ROLL"

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