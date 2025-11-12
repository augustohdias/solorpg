{-# LANGUAGE OverloadedStrings #-}

module System.Action
  ( ActionType (..),
    process,
  )
where

import Control.Concurrent.MVar (modifyMVar_)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Monad (unless, void, when, filterM)
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
import System.ConsequenceContract (Choice (..), Consequence (..), MoveType (..))
import qualified System.ConsequenceContract as Consequence
import qualified System.Constants as C
import qualified System.Dice as Dice
import System.GameContext (BondCommand (..))
import qualified System.GameContext as GameContext
import qualified System.Help as Help
import qualified System.Move as Move
import qualified System.AssetLoader as AssetLoader
import Data.List (find)
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
  = 
    RollDice
  | 
    AddStoryLog
  | 
    Show
  | 
    Save
  | 
    Exit
  | 
    CreateCharacter
  | 
    LoadCharacter
  | 
    ShowCharacter
  | 
    UpdateAttribute
  | 
    UpdateResource
  | 
    AddAttribute
  | 
    AddResource
  | 
    Challenge
  | 
    Move
  | 
    SwearVow
  | 
    CreateCombatTrack
  | 
    CreateJourneyTrack
  | 
    EndCombat
  | 
    ReachDestination
  | 
    MarkProgress
  | 
    RollProgress
  | 
    ShowTracks
  | 
    AbandonTrack
  | 
    Oracle
  | 
    Help
  | 
    Bond
   | 
     AddBonusManually
   | 
     ResolveChoice
   | 
     PickAssetCommand
   | 
     AddAssetCommand
   | 
     ExploreAssets
   | 
     ViewAsset
   | 
     ShowSkillDescription
  | 
    HostSession
  | 
    ConnectSession
  | 
    DisconnectSession
  | 
    SharedVow
  | 
    AcceptConnection
  | 
    RejectConnection
  | 
    Unknown
  deriving (Show, Eq)


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
  CreateJourneyTrack -> createJourneyTrack tuiOutputChannel input
  EndCombat -> endTheFight tuiOutputChannel input
  ReachDestination -> reachDestination tuiOutputChannel input
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
  PickAssetCommand -> pickAsset tuiOutputChannel input
  AddAssetCommand -> addAsset tuiOutputChannel input
  ExploreAssets -> exploreAssets tuiOutputChannel input
  ViewAsset -> viewAsset tuiOutputChannel input
  ShowSkillDescription -> showSkillDescription tuiOutputChannel input
  _ -> defaultAction tuiOutputChannel input


logMessage :: TChan GameOutput -> T.Text -> IO ()
logMessage tuiOutputChannel msg =
  atomically $ writeTChan tuiOutputChannel (LogEntry msg NarrativeMessage)


systemMessage :: TChan GameOutput -> T.Text -> IO ()
systemMessage tuiOutputChannel msg =
  atomically $ writeTChan tuiOutputChannel (LogEntry msg SystemMessage)


withContext :: (GameContext.Context -> IO Bool) -> IO Bool
withContext action = do
  maybeCtx <- GameContext.getCurrentContext
  case maybeCtx of
    Nothing -> pure True
    Just ctx -> action ctx


whenContext :: (GameContext.Context -> IO ()) -> IO ()
whenContext action = do
  maybeCtx <- GameContext.getCurrentContext
  for_ maybeCtx action


defaultAction :: TChan GameOutput -> T.Text -> IO Bool
defaultAction _ _ = return True

isFirstCharAlphaNum :: T.Text -> Bool
isFirstCharAlphaNum text =
  case T.uncons text of
    Nothing -> False
    Just (c, _) -> isAlphaNum c


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

        when (GameContext.isMultiplayer ctx && not (T.isPrefixOf "~" formattedLog)) $ do
            networkState <- NetworkState.getNetworkState
            timestamp <- getCurrentTime
            let playerName = GameContext.getCharacterName ctx

            case networkState of
              NetworkState.ServerState serverState -> do
                let msg = NetworkProtocol.StoryLogEntry playerName formattedLog timestamp
                NetworkServer.broadcastMessage serverState msg Nothing
              NetworkState.ClientState clientState -> do
                let msg = NetworkProtocol.StoryLogEntry playerName formattedLog timestamp
                _ <- NetworkClient.sendMessage clientState msg
                return ()
              NetworkState.NoNetworkState -> return ()
      return True


syncSharedVowCreated :: TChan GameOutput -> GameContext.Context -> T.Text -> Progress.ProgressTrack -> IO ()
syncSharedVowCreated _ ctx vowName track = do
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


syncSharedVowProgress :: TChan GameOutput -> GameContext.Context -> T.Text -> Int -> IO ()
syncSharedVowProgress _ ctx vowName ticks = do
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


syncSharedVowCompleted :: TChan GameOutput -> GameContext.Context -> T.Text -> Int -> IO ()
syncSharedVowCompleted _ ctx vowName experience = do
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


rollDice :: TChan GameOutput -> T.Text -> IO Bool
rollDice tuiOutputChannel diceDescription = do
  rolls <- Dice.roll diceDescription
  let msg = C.msgDiceRolled C.messages <> T.pack (show rolls)
  _ <- process tuiOutputChannel AddStoryLog msg
  return True


showLogs :: TChan GameOutput -> T.Text -> IO Bool
showLogs tuiOutputChannel _ = withContext $ \ctx -> do
  systemMessage tuiOutputChannel (C.msgLogsHeader C.messages)
  let logs = GameContext.getSessionLog ctx
  mapM_ (logMessage tuiOutputChannel) logs
  return True


exit :: TChan GameOutput -> T.Text -> IO Bool
exit tuiOutputChannel _ = do
  systemMessage tuiOutputChannel (C.msgSessionEnded C.messages)
  atomically $ writeTChan tuiOutputChannel GameEnd
  return False




createCharacter :: TChan GameOutput -> T.Text -> IO Bool
createCharacter tuiOutputChannel input = do
  let parts = T.words input
  case parts of
    [] -> systemMessage tuiOutputChannel (C.msgCharacterNameRequired C.messages) >> return True
    (charName : paramParts) -> do
      
      let (assetParts, attrParts) = partition (T.isPrefixOf "assets:") paramParts

      
      playerAssets <- case assetParts of
        [] -> return []
        (assetParam:_) -> parseAssetsParam assetParam

      let attrs = Parser.parseAttributes attrParts
      result <- GameContext.createContext charName attrs
      case result of
        Left err -> handleError err
        Right ctx -> do
          
          finalCtx <- if null playerAssets
            then return ctx
            else addAssetsToCharacter ctx playerAssets

          _ <- GameContext.saveContext finalCtx
          handleSuccess finalCtx
  where
    handleError err = systemMessage tuiOutputChannel (C.msgErrorCreating C.messages <> T.pack (show err)) >> return True
    handleSuccess ctx = do
      let msg = C.msgCharacterCreated C.messages <> GameContext.getCharacterName ctx
      systemMessage tuiOutputChannel msg

      
      let assets = GameContext.assets (GameContext.mainCharacter ctx)
      unless (null assets) $ do
        systemMessage tuiOutputChannel "Assets selecionados:"
        mapM_ (\asset -> systemMessage tuiOutputChannel $ "  - " <> GameContext.playerAssetName asset) assets

      atomically $ writeTChan tuiOutputChannel (CharacterUpdate (GameContext.mainCharacter ctx))
      return True

    
    parseAssetsParam :: T.Text -> IO [GameContext.PlayerAsset]
    parseAssetsParam param = do
      let assetNames = T.splitOn "," (T.drop 7 param) 
      validAssets <- filterM AssetLoader.assetExists assetNames

      
      let invalidAssets = filter (`notElem` validAssets) assetNames
      unless (null invalidAssets) $ systemMessage tuiOutputChannel $ "Assets não encontrados: " <> T.intercalate ", " invalidAssets

      return $ map (\name -> GameContext.PlayerAsset name [1]) validAssets 

    
    addAssetsToCharacter :: GameContext.Context -> [GameContext.PlayerAsset] -> IO GameContext.Context
    addAssetsToCharacter ctx playerAssets = do
      let char = GameContext.mainCharacter ctx
      let updatedChar = char { GameContext.assets = playerAssets }
      return ctx { GameContext.mainCharacter = updatedChar }

    partition :: (a -> Bool) -> [a] -> ([a], [a])
    partition pred' = foldr select ([], [])
      where
        select x (trues, falses)
          | pred' x   = (x:trues, falses)
          | otherwise = (trues, x:falses)


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


showCharacter :: TChan GameOutput -> T.Text -> IO Bool
showCharacter tuiOutputChannel _ = withContext $ \ctx -> do
  let char = GameContext.mainCharacter ctx
  atomically $ writeTChan tuiOutputChannel (CharacterUpdate char)
  return True


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


challenge :: TChan GameOutput -> T.Text -> IO Bool
challenge tuiOutputChannel input = withContext $ \ctx -> do
  let strippedInput = T.strip input
      maybeStat = if T.null strippedInput then Nothing else Move.parseStat strippedInput
      statModifier = case maybeStat of
        Just stat -> 
          let char = GameContext.mainCharacter ctx
              attrs = GameContext.attributes char
          in getStatValue stat attrs
        Nothing -> 0
  
  result <- Dice.challengeRoll
  either handleError (handleSuccess statModifier) result
  where
    getStatValue :: Move.Stat -> GameContext.Attributes -> Int
    getStatValue stat attrs = case stat of
      Move.Iron -> GameContext.iron attrs
      Move.Edge -> GameContext.edge attrs
      Move.Heart -> GameContext.heart attrs
      Move.Shadow -> GameContext.shadow attrs
      Move.Wits -> GameContext.wits attrs
    
    handleError err = systemMessage tuiOutputChannel err >> return True
    handleSuccess modifier challengeResult = do
      let actionDie = Dice.challengeActionDie challengeResult
          ch1 = Dice.challengeDie1 challengeResult
          ch2 = Dice.challengeDie2 challengeResult
          modifiedActionDie = actionDie + modifier
          recalculatedResult = Dice.evaluateActionRoll modifiedActionDie ch1 ch2
          hasMatch = Dice.challengeMatch challengeResult
          modifiedChallengeResult = Dice.ChallengeResult
            { Dice.challengeActionDie = modifiedActionDie
            , Dice.challengeDie1 = ch1
            , Dice.challengeDie2 = ch2
            , Dice.challengeRollResult = recalculatedResult
            , Dice.challengeMatch = hasMatch
            }
      let formattedMsg =
            Dice.formatChallengeResult
              modifiedChallengeResult
              (C.formatActionRoll C.characterDisplay)
              (C.challengeStrongHit C.challengeInterpretation)
              (C.challengeWeakHit C.challengeInterpretation)
              (C.challengeMiss C.challengeInterpretation)
              (C.challengeMatch C.challengeInterpretation)
      _ <- process tuiOutputChannel AddStoryLog formattedMsg
      return True


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


parseMoveParams :: [T.Text] -> (Maybe Move.Stat, T.Text)
parseMoveParams [] = (Nothing, "")
parseMoveParams params@(first : rest) = (stat, text)
  where
    stat = Move.parseStat first
    text = maybe (T.unwords params) (const (T.unwords rest)) stat


addAssetToPlayerContext :: TChan GameOutput -> T.Text -> IO ()
addAssetToPlayerContext tuiOutputChannel assetName = do
  maybeCtx <- GameContext.getCurrentContext
  case maybeCtx of
    Nothing -> do
      systemMessage tuiOutputChannel "Nenhum personagem carregado."
      return ()
    Just ctx -> do
      
      assetExists <- AssetLoader.assetExists assetName
      if not assetExists
        then do
          systemMessage tuiOutputChannel $ "Asset não encontrado: " <> assetName
          return ()
        else do
          
          let playerAssets = GameContext.assets (GameContext.mainCharacter ctx)
          let alreadyHas = any (\pa -> GameContext.playerAssetName pa == assetName) playerAssets
          if alreadyHas
            then do
              systemMessage tuiOutputChannel $ "Você já possui o asset: " <> assetName
              return ()
            else do
              
              let newAsset = GameContext.PlayerAsset assetName [1] 
              let char = GameContext.mainCharacter ctx
              let newChar = char { GameContext.assets = newAsset : playerAssets }
              let updatedCtx = ctx { GameContext.mainCharacter = newChar }

              _ <- GameContext.saveContext updatedCtx
              systemMessage tuiOutputChannel $ "Asset adquirido: " <> assetName
              atomically $ writeTChan tuiOutputChannel (CharacterUpdate newChar)
              return ()


processConsequences :: TChan GameOutput -> [Consequence] -> GameContext.Context -> IO ()
processConsequences _ [] _ = pure ()
processConsequences tuiOutputChannel (current : rest) ctx = do
  shouldPause <- processConsequence tuiOutputChannel ctx current rest
  unless shouldPause (processConsequences tuiOutputChannel rest ctx)


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
  MarkJourneyProgress -> do
    markJourneyProgressTrack tuiOutputChannel ctx
    return False
  Consequence.ImproveAsset assetName -> do
    
    let currentXP = GameContext.experience (GameContext.resources (GameContext.mainCharacter ctx))
    if currentXP < 1
      then do
        systemMessage tuiOutputChannel "Experiência insuficiente. Custa 1 XP para melhorar um asset."
        return False
      else do
        successful <- improvePlayerAsset tuiOutputChannel ctx assetName
        if successful
          then do
            
            _ <- process tuiOutputChannel AddResource "experience:-1"
            systemMessage tuiOutputChannel "Gastou 1 XP para melhorar o asset."
            return False
          else return False 
  Consequence.ChooseAssetImprovement description -> do
    
    let currentXP = GameContext.experience (GameContext.resources (GameContext.mainCharacter ctx))
    if currentXP < 2
      then do
        systemMessage tuiOutputChannel "Experiência insuficiente. Custa 2 XP para melhorar um asset."
        return False
      else do
        successful <- presentAssetImprovementChoice tuiOutputChannel description remaining ctx
        if successful
          then return True  -- Pausa para aguardar escolha do usuário
          else return False
  Consequence.PickAsset description -> do
    
    let currentXP = GameContext.experience (GameContext.resources (GameContext.mainCharacter ctx))
    if currentXP < 2
      then do
        systemMessage tuiOutputChannel "Experiência insuficiente. Custa 2 XP para adquirir um novo asset."
        return False
      else do
        successful <- presentPickAssetChoice tuiOutputChannel description remaining
        if successful
          then do
            
            return True 
          else return True 
  Consequence.AddAssetToPlayer assetName -> do
    addAssetToPlayerContext tuiOutputChannel assetName
    return False
  Consequence.DeductExperience amount -> do
    _ <- process tuiOutputChannel AddResource ("experience:-" <> T.pack (show amount))
    systemMessage tuiOutputChannel $ "Gastou " <> T.pack (show amount) <> " XP."
    return False
  Consequence.ImproveSpecificAssetSkill assetName skillIndex -> do
    successful <- improveSpecificAssetSkill tuiOutputChannel ctx assetName skillIndex
    if successful
      then do
        systemMessage tuiOutputChannel $ "Asset " <> assetName <> " melhorado com sucesso!"
        return False
      else do
        systemMessage tuiOutputChannel $ "Erro ao melhorar o asset " <> assetName <> "."
        return False


presentPlayerChoice :: TChan GameOutput -> [Choice] -> [Consequence] -> IO ()
presentPlayerChoice tuiOutputChannel choices remaining = do
  promptIdSeed <- randomIO :: IO Int
  let payload = Parser.buildChoicePromptPayload promptIdSeed choices remaining
  atomically $ writeTChan tuiOutputChannel (ChoicePrompt payload)
  systemMessage tuiOutputChannel "Escolha uma opção para continuar o movimento."


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


markJourneyProgressTrack :: TChan GameOutput -> GameContext.Context -> IO ()
markJourneyProgressTrack tuiOutputChannel _ctx = do
  maybeCurrentCtx <- GameContext.getCurrentContext
  case maybeCurrentCtx of
    Nothing -> return ()
    Just currentCtx -> do
      -- Procura o journey track ativo mais recente
      let journeyTracks = filter (\t -> Progress.trackType t == Progress.Journey && not (Progress.trackCompleted t)) (GameContext.progressTracks currentCtx)
      
      case journeyTracks of
        [] -> do
          systemMessage tuiOutputChannel "Nenhuma jornada ativa encontrada. Use :journey para criar uma jornada."
          return ()
        (track:_) -> do
          -- Marca progresso no journey track
          updatedTrack <- Progress.markProgress track
          
          updatedCtx <- GameContext.updateProgressTrack currentCtx (Progress.trackName track) updatedTrack
          _ <- GameContext.saveContext updatedCtx

          let boxes = Progress.getProgressScore updatedTrack
          let ticks = Progress.trackTicks updatedTrack
          logMessage tuiOutputChannel $ "[+] Journey progress: " <> Progress.trackName track <> " (" <> T.pack (show boxes) <> "/10 boxes, " <> T.pack (show ticks) <> "/40 ticks)"

          -- Sincronizar em multiplayer se necessário
          when (GameContext.isMultiplayer updatedCtx) $ syncProgressTrack tuiOutputChannel updatedCtx (Progress.trackName track) updatedTrack


improvePlayerAsset :: TChan GameOutput -> GameContext.Context -> T.Text -> IO Bool
improvePlayerAsset tuiOutputChannel ctx assetName = do
  let playerAssets = GameContext.assets (GameContext.mainCharacter ctx)
  case find (\pa -> GameContext.playerAssetName pa == assetName) playerAssets of
    Nothing -> do
      systemMessage tuiOutputChannel $ "Você não possui o asset: " <> assetName
      return False
    Just playerAsset -> do
      
      maybeBaseAsset <- AssetLoader.findAssetByName assetName
      case maybeBaseAsset of
        Nothing -> do
          systemMessage tuiOutputChannel $ "Definição do asset não encontrada: " <> assetName
          return False
        Just baseAsset -> do
          let currentSkills = GameContext.enabledSkills playerAsset
          let maxSkills = length (GameContext.assetSkills baseAsset)
          let nextSkillIndex = maximum currentSkills + 1

          if nextSkillIndex > maxSkills
            then do
              systemMessage tuiOutputChannel $ "Asset " <> assetName <> " já está totalmente melhorado."
              return False
            else do
              
              let updatedSkills = nextSkillIndex : currentSkills
              let updatedAsset = playerAsset { GameContext.enabledSkills = updatedSkills }
              let otherAssets = filter (\pa -> GameContext.playerAssetName pa /= assetName) playerAssets
              let char = GameContext.mainCharacter ctx
              let newChar = char { GameContext.assets = updatedAsset : otherAssets }
              let updatedCtx = ctx { GameContext.mainCharacter = newChar }

              _ <- GameContext.saveContext updatedCtx
              systemMessage tuiOutputChannel $ "Asset melhorado: " <> assetName <> " (habilidade " <> T.pack (show nextSkillIndex) <> " habilitada)"
              atomically $ writeTChan tuiOutputChannel (CharacterUpdate newChar)
              return True


presentPickAssetChoice :: TChan GameOutput -> T.Text -> [Consequence] -> IO Bool
presentPickAssetChoice tuiOutputChannel _ remaining = do
  availableAssets <- AssetLoader.getAvailableAssets
  if null availableAssets
    then do
      systemMessage tuiOutputChannel "Nenhum asset disponível para escolha."
      return False
    else do
      let assetChoices = map createAssetChoice availableAssets
      presentPlayerChoice tuiOutputChannel assetChoices remaining
      return True
  where
    createAssetChoice :: GameContext.Asset -> Consequence.Choice
    createAssetChoice asset =
      Consequence.Choice
        { Consequence.choiceDescription = GameContext.assetName asset
        , Consequence.choiceConsequences =
            [ Consequence.Narrative $ "Agora eu tenho " <> GameContext.assetName asset
            , Consequence.AddAssetToPlayer (GameContext.assetName asset)
            , Consequence.DeductExperience 2  
            ]
        }


presentAssetImprovementChoice :: TChan GameOutput -> T.Text -> [Consequence] -> GameContext.Context -> IO Bool
presentAssetImprovementChoice tuiOutputChannel _description remaining ctx = do
  improvementOptions <- getAssetImprovementOptions ctx
  if null improvementOptions
    then do
      systemMessage tuiOutputChannel "Nenhum asset pode ser melhorado no momento."
      return False
    else do
      let improvementChoices = map createImprovementChoice improvementOptions
      presentPlayerChoice tuiOutputChannel improvementChoices remaining
      return True
  where
    createImprovementChoice :: (T.Text, T.Text, Int) -> Consequence.Choice
    createImprovementChoice (assetName, skillName, skillIndex) =
      Consequence.Choice
        { Consequence.choiceDescription = assetName <> ", " <> skillName
        , Consequence.choiceConsequences =
            [ Consequence.Narrative $ "Melhorei " <> assetName <> ", " <> skillName
            , Consequence.ImproveSpecificAssetSkill assetName skillIndex
            , Consequence.DeductExperience 2
            ]
        }


getAssetImprovementOptions :: GameContext.Context -> IO [(T.Text, T.Text, Int)]
getAssetImprovementOptions ctx = do
  let playerAssets = GameContext.assets (GameContext.mainCharacter ctx)
  improvementOptions <- mapM getAssetOptions playerAssets
  return $ concat improvementOptions
  where
    getAssetOptions :: GameContext.PlayerAsset -> IO [(T.Text, T.Text, Int)]
    getAssetOptions playerAsset = do
      let assetName = GameContext.playerAssetName playerAsset
      maybeBaseAsset <- AssetLoader.findAssetByName assetName
      case maybeBaseAsset of
        Nothing -> return []
        Just baseAsset -> do
          let currentSkills = GameContext.enabledSkills playerAsset
          let allSkills = GameContext.assetSkills baseAsset
          let availableSkills = filter (\(idx, _) -> idx `notElem` currentSkills) (zip [1..] allSkills)
          return $ map (\(idx, skill) -> (assetName, GameContext.skillName skill, idx)) availableSkills


improveSpecificAssetSkill :: TChan GameOutput -> GameContext.Context -> T.Text -> Int -> IO Bool
improveSpecificAssetSkill tuiOutputChannel ctx assetName skillIndex = do
  let playerAssets = GameContext.assets (GameContext.mainCharacter ctx)
  case find (\pa -> GameContext.playerAssetName pa == assetName) playerAssets of
    Nothing -> do
      systemMessage tuiOutputChannel $ "Você não possui o asset: " <> assetName
      return False
    Just playerAsset -> do
      -- Verifica se o asset base existe
      maybeBaseAsset <- AssetLoader.findAssetByName assetName
      case maybeBaseAsset of
        Nothing -> do
          systemMessage tuiOutputChannel $ "Definição do asset não encontrada: " <> assetName
          return False
        Just baseAsset -> do
          let currentSkills = GameContext.enabledSkills playerAsset
          let allSkills = GameContext.assetSkills baseAsset
          let maxSkills = length allSkills
          
          -- Validações
          if skillIndex < 1 || skillIndex > maxSkills
            then do
              systemMessage tuiOutputChannel $ "Índice de habilidade inválido: " <> T.pack (show skillIndex)
              return False
            else if skillIndex `elem` currentSkills
              then do
                systemMessage tuiOutputChannel $ "Esta habilidade já está habilitada no asset " <> assetName
                return False
              else do
                -- Melhora o asset adicionando a nova skill
                let updatedSkills = skillIndex : currentSkills
                let updatedAsset = playerAsset { GameContext.enabledSkills = updatedSkills }
                let otherAssets = filter (\pa -> GameContext.playerAssetName pa /= assetName) playerAssets
                let char = GameContext.mainCharacter ctx
                let newChar = char { GameContext.assets = updatedAsset : otherAssets }
                let updatedCtx = ctx { GameContext.mainCharacter = newChar }
                
                -- Salva no contexto e no savefile
                _ <- GameContext.saveContext updatedCtx
                
                -- Atualiza o cache de contexto em memória
                cache <- GameContext.getContextCache
                modifyMVar_ cache $ \_ -> return (Just updatedCtx)
                
                -- Notifica a UI sobre a atualização do personagem
                atomically $ writeTChan tuiOutputChannel (CharacterUpdate newChar)
                
                -- Obtém o nome da skill melhorada
                let skillName = case drop (skillIndex - 1) allSkills of
                      [] -> "Habilidade " <> T.pack (show skillIndex)
                      (skill:_) -> GameContext.skillName skill
                
                systemMessage tuiOutputChannel $ "Asset " <> assetName <> " melhorado: " <> skillName <> " habilitada"
                return True


pickAsset :: TChan GameOutput -> T.Text -> IO Bool
pickAsset tuiOutputChannel input = withContext $ \ctx -> do
  let assetName = T.strip input
  if T.null assetName
    then do
      systemMessage tuiOutputChannel "Use: :pickasset <Nome do Asset>"
      return True
    else do
      
      assetExists <- AssetLoader.assetExists assetName
      if not assetExists
        then do
          systemMessage tuiOutputChannel $ "Asset não encontrado: " <> assetName
          return True
        else do
          
          let playerAssets = GameContext.assets (GameContext.mainCharacter ctx)
          let alreadyHas = any (\pa -> GameContext.playerAssetName pa == assetName) playerAssets
          if alreadyHas
            then do
              systemMessage tuiOutputChannel $ "Você já possui o asset: " <> assetName
              return True
            else do
              
              let currentXP = GameContext.experience (GameContext.resources (GameContext.mainCharacter ctx))
              if currentXP < 2
                then do
                  systemMessage tuiOutputChannel "Experiência insuficiente. Custa 2 XP para adquirir um asset."
                  return True
                else do
                  
                  let newAsset = GameContext.PlayerAsset assetName [1] 
                  let char = GameContext.mainCharacter ctx
                  let oldRes = GameContext.resources char
                  let newRes = oldRes { GameContext.experience = currentXP - 2 }
                  let newChar = char { GameContext.assets = newAsset : playerAssets
                                     , GameContext.resources = newRes }
                  let updatedCtx = ctx { GameContext.mainCharacter = newChar }

                  _ <- GameContext.saveContext updatedCtx
                  systemMessage tuiOutputChannel $ "Asset adquirido: " <> assetName <> " (custou 2 XP)"
                  atomically $ writeTChan tuiOutputChannel (CharacterUpdate newChar)
                  return True


addAsset :: TChan GameOutput -> T.Text -> IO Bool
addAsset tuiOutputChannel input = withContext $ \ctx -> do
  let assetName = T.strip input
  if T.null assetName
    then do
      systemMessage tuiOutputChannel "Use: :addasset <Nome do Asset>"
      return True
    else do
      
      assetExists <- AssetLoader.assetExists assetName
      if not assetExists
        then do
          systemMessage tuiOutputChannel $ "Asset não encontrado: " <> assetName
          return True
        else do
          
          let playerAssets = GameContext.assets (GameContext.mainCharacter ctx)
          let alreadyHas = any (\pa -> GameContext.playerAssetName pa == assetName) playerAssets
          if alreadyHas
            then do
              systemMessage tuiOutputChannel $ "Você já possui o asset: " <> assetName
              return True
            else do
              
              let newAsset = GameContext.PlayerAsset assetName [1] 
              let char = GameContext.mainCharacter ctx
              let newChar = char { GameContext.assets = newAsset : playerAssets }
              let updatedCtx = ctx { GameContext.mainCharacter = newChar }

              _ <- GameContext.saveContext updatedCtx
              systemMessage tuiOutputChannel $ "Asset adicionado: " <> assetName
              atomically $ writeTChan tuiOutputChannel (CharacterUpdate newChar)
              return True


exploreAssets :: TChan GameOutput -> T.Text -> IO Bool
exploreAssets tuiOutputChannel _input = do
  assets <- AssetLoader.getAvailableAssets
  if null assets
    then do
      systemMessage tuiOutputChannel "Nenhum asset carregado."
      return True
    else do
      
      atomically $ writeTChan tuiOutputChannel (AssetExploreRequest assets)
      return True


viewAsset :: TChan GameOutput -> T.Text -> IO Bool
viewAsset tuiOutputChannel input = do
  let assetName = T.strip input
  if T.null assetName
    then do
      systemMessage tuiOutputChannel "Use: :viewasset <Nome do Asset>"
      return True
    else do
      maybeAsset <- AssetLoader.findAssetByName assetName
      case maybeAsset of
        Nothing -> do
          systemMessage tuiOutputChannel $ "Asset não encontrado: " <> assetName
          return True
        Just asset -> do
          
          atomically $ writeTChan tuiOutputChannel (AssetViewRequest asset)
          return True


showSkillDescription :: TChan GameOutput -> T.Text -> IO Bool
showSkillDescription tuiOutputChannel input = do
  
  let parseQuotedArgs text =
        let stripped = T.strip text
            parts = T.splitOn "\" \"" stripped
        in case parts of
          [first, second] ->
            let cleanFirst = T.strip $ T.replace "\"" "" first
                cleanSecond = T.strip $ T.replace "\"" "" second
            in Just (cleanFirst, cleanSecond)
          _ -> Nothing

  case parseQuotedArgs input of
    Just (assetName, skillName) -> do
      let cleanAssetName = T.strip assetName
      let cleanSkillName = T.strip skillName

      maybeAsset <- AssetLoader.findAssetByName cleanAssetName
      case maybeAsset of
        Nothing -> do
          systemMessage tuiOutputChannel $ "Asset não encontrado: " <> cleanAssetName
          return True
        Just asset -> do
          let skills = GameContext.assetSkills asset
          case find (\skill -> GameContext.skillName skill == cleanSkillName) skills of
            Nothing -> do
              systemMessage tuiOutputChannel $ "Habilidade não encontrada: " <> cleanSkillName
              return True
            Just skill -> do
              systemMessage tuiOutputChannel $ "=== " <> GameContext.skillName skill <> " ==="
              systemMessage tuiOutputChannel $ GameContext.skillDescription skill
              return True
    Nothing -> do
      systemMessage tuiOutputChannel "Erro ao processar comando de habilidade."
      return True


applyResourceDelta :: TChan GameOutput -> T.Text -> Int -> IO ()
applyResourceDelta tuiOutputChannel resourceName delta = do
  let command = resourceName <> ":" <> deltaText
  void (process tuiOutputChannel AddResource command)
  where
    deltaText
      | delta >= 0 = "+" <> T.pack (show delta)
      | otherwise = T.pack (show delta)


oracleQuery :: TChan GameOutput -> T.Text -> IO Bool
oracleQuery tuiOutputChannel input = if T.null (T.strip input)
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


rollOracleWithGivenValue :: TChan GameOutput -> T.Text -> T.Text -> IO Bool
rollOracleWithGivenValue tuiOutputChannel oracleName valueText = case TR.decimal valueText of
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


executeOracleConsequences :: TChan GameOutput -> Oracle.OracleResult -> IO ()
executeOracleConsequences tuiOutputChannel result = do
  let structuredConsequences = Oracle.resultConsequences result
  unless (null structuredConsequences) $ do
    maybeCtx <- GameContext.getCurrentContext
    for_ maybeCtx $ \ctx ->
      processConsequences tuiOutputChannel structuredConsequences ctx


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


swearVow :: TChan GameOutput -> T.Text -> IO Bool
swearVow tuiOutputChannel input = withContext $ \ctx -> maybe handleParseError (processVow ctx) (Parser.parseQuotedString input)
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


createCombatTrack :: TChan GameOutput -> T.Text -> IO Bool
createCombatTrack tuiOutputChannel input = withContext $ \ctx -> maybe handleParseError (processCombat ctx) (Parser.parseQuotedString input)
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

      
      when (GameContext.isMultiplayer updatedCtx) $ syncProgressTrack tuiOutputChannel updatedCtx enemyName track

      return True


createJourneyTrack :: TChan GameOutput -> T.Text -> IO Bool
createJourneyTrack tuiOutputChannel input = withContext $ \ctx -> maybe handleParseError (processJourney ctx) (Parser.parseQuotedString input)
  where
    handleParseError = systemMessage tuiOutputChannel (T.pack (C.msgJourneyTrackUsage C.moveMessages)) >> return True
    processJourney ctx (destination, rest) = maybe (handleInvalidRank rest) (createJourney ctx destination) (Parser.parseRank (T.strip rest))
    handleInvalidRank rest = do
      systemMessage tuiOutputChannel $ T.pack (C.errInvalidRank C.errorMessages) <> T.strip rest
      systemMessage tuiOutputChannel $ T.pack (C.msgJourneyTrackUsage C.moveMessages)
      return True
    createJourney ctx destination rank = do
      let track = Progress.newProgressTrack destination Progress.Journey rank
      let ticks = Progress.getTicksForRank rank
      updatedCtx <- GameContext.addProgressTrack ctx track
      _ <- GameContext.saveContext updatedCtx
      let formattedMsg = T.pack $ C.formatJourneyTrackCreated C.moveMessages destination (T.unpack $ Parser.rankToText rank) ticks
      _ <- process tuiOutputChannel AddStoryLog formattedMsg

      -- Sync para multiplayer se necessário
      when (GameContext.isMultiplayer updatedCtx) $ syncProgressTrack tuiOutputChannel updatedCtx destination track

      return True


endTheFight :: TChan GameOutput -> T.Text -> IO Bool
endTheFight tuiOutputChannel input = withContext $ \ctx -> do
  let trackName = T.strip input
  case nonEmpty trackName of
    Nothing -> do
      systemMessage tuiOutputChannel "Uso: :endfight \"<nome da combat track>\""
      systemMessage tuiOutputChannel "Exemplo: :endfight \"Bandidos\""
      return True
    Just name -> handleCombatTrack ctx name
  where
    handleCombatTrack ctx name = case GameContext.getProgressTrack ctx name of
      Nothing -> systemMessage tuiOutputChannel (C.msgTrackNotFound C.moveMessages <> name) >> return True
      Just track -> 
        if Progress.trackType track /= Progress.Combat
          then do
            systemMessage tuiOutputChannel $ "\"" <> name <> "\" não é uma combat track."
            return True
          else processCombatTrack ctx name track

    processCombatTrack ctx name track
      | Progress.trackCompleted track = systemMessage tuiOutputChannel "Combat track já está completo!" >> return True
      | otherwise = do
          let progressScore = Progress.getProgressScore track  -- Número de boxes preenchidas
          
          -- Rolar dados de desafio para o progress roll
          challengeRolls <- Dice.roll (T.pack "2d10")
          case challengeRolls of
            [(_, ch1), (_, ch2)] -> do
              -- Fazer progress roll usando o movimento EndTheFight  
              let char = GameContext.mainCharacter ctx
              let attrs = GameContext.attributes char
              let resources = GameContext.resources char
              consequences <- Move.executeMoveWithRoll EndTheFight Nothing progressScore (ch1, ch2) attrs resources
              
              -- Processar consequências
              processConsequences tuiOutputChannel consequences ctx
              
              -- Se foi bem-sucedido, marcar a track como completa e dar experiência
              let wasSuccess = any isSuccessConsequence consequences
              when wasSuccess $ do
                completedTrack <- Progress.completeTrack track
                updatedCtx <- GameContext.updateProgressTrack ctx name completedTrack
                _ <- GameContext.saveContext updatedCtx
                
                -- Dar experiência baseada no rank
                let expGained = Progress.getRankExperienceValue (Progress.trackRank track)
                _ <- process tuiOutputChannel AddResource ("experience:+" <> T.pack (show expGained))
                systemMessage tuiOutputChannel $ "Combat concluído! Ganhou " <> T.pack (show expGained) <> " XP."
              
              return True
            _ -> do
              systemMessage tuiOutputChannel "Erro ao rolar dados de desafio."
              return True
    
    -- Verifica se houve sucesso (Strong Hit ou Weak Hit)
    isSuccessConsequence :: Consequence -> Bool
    isSuccessConsequence (Narrative text) = 
      T.isInfixOf "vence decisivamente" text || T.isInfixOf "vence, mas" text
    isSuccessConsequence _ = False
    
    nonEmpty s = if T.null s then Nothing else Just s


reachDestination :: TChan GameOutput -> T.Text -> IO Bool
reachDestination tuiOutputChannel input = withContext $ \ctx -> do
  let trackName = T.strip input
  case nonEmpty trackName of
    Nothing -> do
      systemMessage tuiOutputChannel "Uso: :reachdestination \"<nome da journey track>\""
      systemMessage tuiOutputChannel "Exemplo: :reachdestination \"Vila Próxima\""
      return True
    Just name -> handleJourneyTrack ctx name
  where
    handleJourneyTrack ctx name = case GameContext.getProgressTrack ctx name of
      Nothing -> systemMessage tuiOutputChannel (C.msgTrackNotFound C.moveMessages <> name) >> return True
      Just track -> 
        if Progress.trackType track /= Progress.Journey
          then do
            systemMessage tuiOutputChannel $ "\"" <> name <> "\" não é uma journey track."
            return True
          else processJourneyTrack ctx name track

    processJourneyTrack ctx name track
      | Progress.trackCompleted track = systemMessage tuiOutputChannel "Journey track já está completo!" >> return True
      | otherwise = do
          let progressScore = Progress.getProgressScore track  -- Número de boxes preenchidas
          
          -- Rolar dados de desafio para o progress roll
          challengeRolls <- Dice.roll (T.pack "2d10")
          case challengeRolls of
            [(_, ch1), (_, ch2)] -> do
              -- Fazer progress roll usando o movimento ReachYourDestination  
              let char = GameContext.mainCharacter ctx
              let attrs = GameContext.attributes char
              let resources = GameContext.resources char
              consequences <- Move.executeMoveWithRoll ReachYourDestination Nothing progressScore (ch1, ch2) attrs resources
              
              -- Processar consequências
              processConsequences tuiOutputChannel consequences ctx
              
              -- Se foi bem-sucedido, marcar a track como completa e dar experiência
              let wasSuccess = any isSuccessConsequence consequences
              when wasSuccess $ do
                completedTrack <- Progress.completeTrack track
                updatedCtx <- GameContext.updateProgressTrack ctx name completedTrack
                _ <- GameContext.saveContext updatedCtx
                
                -- Dar experiência baseada no rank
                let expGained = Progress.getRankExperienceValue (Progress.trackRank track)
                _ <- process tuiOutputChannel AddResource ("experience:+" <> T.pack (show expGained))
                systemMessage tuiOutputChannel $ "Jornada concluída! Ganhou " <> T.pack (show expGained) <> " XP."
              
              return True
            _ -> do
              systemMessage tuiOutputChannel "Erro ao rolar dados de desafio."
              return True
    
    -- Verifica se houve sucesso (Strong Hit ou Weak Hit)
    isSuccessConsequence :: Consequence -> Bool
    isSuccessConsequence (Narrative text) = 
      T.isInfixOf "chega ao seu destino" text || T.isInfixOf "chega ao destino" text
    isSuccessConsequence _ = False
    
    nonEmpty s = if T.null s then Nothing else Just s


markProgress :: TChan GameOutput -> T.Text -> IO Bool
markProgress tuiOutputChannel input = withContext $ \ctx -> do
  case parseProgressInput input of
    Nothing -> do
      systemMessage tuiOutputChannel "Uso: :progress \"<nome do track>\" [número de ticks]"
      systemMessage tuiOutputChannel "Exemplos: :progress \"Piratas\" 15  ou  :progress \"Vila Próxima\""
      return True
    Just (trackName, maybeTicks) -> handleTrackName ctx trackName maybeTicks
  where
    -- Parse input para separar nome da track do número de ticks opcional
    parseProgressInput :: T.Text -> Maybe (T.Text, Maybe Int)
    parseProgressInput txt = 
      case Parser.parseQuotedString (T.strip txt) of
        Just (name, rest) -> 
          let ticksText = T.strip rest
          in if T.null ticksText
               then Just (name, Nothing)
               else case TR.decimal ticksText of
                 Right (n, _) -> Just (name, Just n)
                 Left _ -> Just (name, Nothing)  -- Ignora texto inválido e usa progresso padrão
        Nothing -> 
          let parts = T.words (T.strip txt)
          in case parts of
               [] -> Nothing
               [name] -> Just (name, Nothing)
               (name:ticksStr:_) -> case TR.decimal ticksStr of
                 Right (n, _) -> Just (name, Just n)
                 Left _ -> Just (name, Nothing)

    handleTrackName ctx name maybeTicks = case GameContext.getProgressTrack ctx name of
      Nothing -> systemMessage tuiOutputChannel (C.msgTrackNotFound C.moveMessages <> name) >> return True
      Just track -> processTrack ctx name track maybeTicks

    processTrack ctx name track maybeTicks
      | Progress.trackCompleted track = systemMessage tuiOutputChannel "Track já está completo!" >> return True
      | otherwise = do
          updatedTrack <- case maybeTicks of
            Nothing -> Progress.markProgress track  -- Progresso padrão baseado no rank
            Just ticks -> Progress.markProgressTicks track ticks  -- Número específico de ticks
          
          updatedCtx <- GameContext.updateProgressTrack ctx name updatedTrack
          _ <- GameContext.saveContext updatedCtx
          let boxes = Progress.getProgressScore updatedTrack
          let totalTicks = Progress.trackTicks updatedTrack
          let progressType = case maybeTicks of
                Nothing -> "progresso padrão"
                Just ticks -> T.pack (show ticks) <> " ticks"
          let msg =
                "[+] Progresso marcado em "
                  <> name
                  <> " ("
                  <> progressType
                  <> "): "
                  <> T.pack (show boxes)
                  <> "/10 boxes ("
                  <> T.pack (show totalTicks)
                  <> "/40 ticks)"
          _ <- process tuiOutputChannel AddStoryLog msg

          -- Multiplayer sync
          when (GameContext.isMultiplayer updatedCtx) $ case Progress.trackType track of
            Progress.Combat -> syncProgressTrack tuiOutputChannel updatedCtx name updatedTrack
            Progress.Journey -> syncProgressTrack tuiOutputChannel updatedCtx name updatedTrack
            Progress.Vow -> do
              let oldTicks = Progress.trackTicks track
              let newTicks = Progress.trackTicks updatedTrack
              when (newTicks > oldTicks) $ do
                syncSharedVowProgress tuiOutputChannel updatedCtx name (newTicks - oldTicks)
            _ -> return ()

          return True


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

          
          when (GameContext.isMultiplayer finalCtx && Progress.trackType track == Progress.Vow && Progress.executionExperienceGained executionResult > 0) $ syncSharedVowCompleted tuiOutputChannel finalCtx name (Progress.executionExperienceGained executionResult)
            

          return True

    applyExperienceGain ctx expGained
      | expGained > 0 = do
          let command = "experience:+" <> T.pack (show expGained)
          _ <- process tuiOutputChannel AddResource command
          fromMaybe ctx <$> GameContext.getCurrentContext
      | otherwise = return ctx

    nonEmpty s = if T.null s then Nothing else Just s


showTracks :: TChan GameOutput -> T.Text -> IO Bool
showTracks tuiOutputChannel _ = withContext $ \ctx -> do
  let tracks = GameContext.progressTracks ctx
  if null tracks
    then systemMessage tuiOutputChannel $ T.pack (C.msgNoTracksActive C.moveMessages)
    else do
      systemMessage tuiOutputChannel $ T.pack (C.msgTracksHeader C.moveMessages)
      mapM_ (systemMessage tuiOutputChannel . Parser.formatProgressTrack) tracks
  return True


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


addBonusCommand :: TChan GameOutput -> T.Text -> IO Bool
addBonusCommand tuiOutputChannel input = withContext $ \ctx -> do
  case Parser.parseBonusCommand input of
    Nothing -> do
      systemMessage tuiOutputChannel "Formato inválido. Use: :bonus <tipo> <valor> [descrição]"
      systemMessage tuiOutputChannel "Tipos: nextroll, nextmove:<nome>, persistent"
      systemMessage tuiOutputChannel "Exemplos:"
      systemMessage tuiOutputChannel "  :bonus nextroll +1 \"Preparado\""
      systemMessage tuiOutputChannel "  :bonus nextmove:EnfrentarPerigo +2 \"Vantagem\""
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

      
      result <- NetworkServer.startServer NetworkServer.defaultPort playerName characterName (Just tuiOutputChannel)
      case result of
        Left err -> do
          systemMessage tuiOutputChannel $ "Erro ao iniciar servidor: " <> T.pack err
          return True
        Right serverState -> do
          
          NetworkState.setNetworkState (NetworkState.ServerState serverState)

          
          let updatedCtx =
                ctx
                  { GameContext.isMultiplayer = True,
                    GameContext.multiplayerSessionId = Just (T.pack $ "session-" ++ show NetworkServer.defaultPort)
                  }
          void $ GameContext.saveContext updatedCtx
          
          cache <- GameContext.getContextCache
          modifyMVar_ cache $ \_ -> return (Just updatedCtx)

          
          maybeIP <- NetworkServer.getLocalIP
          let ipStr = fromMaybe "localhost" maybeIP

          
          systemMessage tuiOutputChannel $ "Servidor iniciado na porta " <> T.pack (show NetworkServer.defaultPort)
          systemMessage tuiOutputChannel $ "IP: " <> T.pack ipStr <> ":" <> T.pack (show NetworkServer.defaultPort)
          systemMessage tuiOutputChannel "Aguardando conexões..."

          return True


connectSession :: TChan GameOutput -> T.Text -> IO Bool
connectSession tuiOutputChannel input = do
  maybeCtx <- GameContext.getCurrentContext
  case maybeCtx of
    Nothing -> do
      systemMessage tuiOutputChannel "Erro: Nenhum personagem carregado. Use :load para carregar um personagem."
      return True
    Just ctx -> do
      
      let parts = T.splitOn ":" (T.strip input)
      if length parts == 2
        then do
          let host = T.unpack (head parts)
          let portStr = parts !! 1
          case TR.decimal portStr :: Either String (Integer, T.Text) of
            Right (port, _) -> do
              let playerName = GameContext.getCharacterName ctx
              let characterName = GameContext.name (GameContext.mainCharacter ctx)

              
              result <- NetworkClient.connectToServer host (fromIntegral port) playerName characterName
              case result of
                Left err -> do
                  systemMessage tuiOutputChannel $ "Erro ao conectar: " <> T.pack err
                  return True
                Right clientState -> do
                  
                  NetworkState.setNetworkState (NetworkState.ClientState clientState)

                  
                  let updatedCtx =
                        ctx
                          { GameContext.isMultiplayer = True,
                            GameContext.multiplayerSessionId = Just (T.pack $ host ++ ":" ++ show port)
                          }

                  
                  cache <- GameContext.getContextCache
                  modifyMVar_ cache $ \_ -> return (Just updatedCtx)
                  void $ GameContext.saveContext updatedCtx

                  
                  NetworkClient.startReceiveLoop clientState tuiOutputChannel

                  systemMessage tuiOutputChannel "Conectado ao servidor!"
                  return True
            Left _ -> do
              systemMessage tuiOutputChannel "Formato inválido. Use: :connect <ip:port>"
              return True
        else do
          systemMessage tuiOutputChannel "Formato inválido. Use: :connect <ip:port>"
          return True


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
    NetworkState.NoNetworkState -> systemMessage tuiOutputChannel "Não há sessão multiplayer ativa."
  return True


sharedVow :: TChan GameOutput -> T.Text -> IO Bool
sharedVow tuiOutputChannel input = withContext $ \ctx -> do
  
  if not (GameContext.isMultiplayer ctx)
    then do
      systemMessage tuiOutputChannel "Shared vows só podem ser criados em modo multiplayer."
      return True
    else maybe handleParseError (processSharedVow ctx) (Parser.parseQuotedString input)
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

      
      syncSharedVowCreated tuiOutputChannel updatedCtx vowName track

      return True



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
        Just conn -> systemMessage tuiOutputChannel $ "Conexão aceita: " <> NetworkTypes.connPlayerName conn <> " (" <> NetworkTypes.connCharacterName conn <> ")"
          
          
      return True
    _ -> do
      systemMessage tuiOutputChannel "Apenas o host pode aceitar conexões."
      return True



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


syncProgressTrack :: TChan GameOutput -> GameContext.Context -> T.Text -> Progress.ProgressTrack -> IO ()
syncProgressTrack _ ctx trackName track = do
  
  when (Progress.trackType track == Progress.Combat || Progress.trackType track == Progress.Journey) $ do
    networkState <- NetworkState.getNetworkState
    let playerName = GameContext.getCharacterName ctx
    let trackData = TE.decodeUtf8 $ BL.toStrict (Aeson.encode track)  

    case networkState of
      NetworkState.ServerState serverState -> do
        let msg = NetworkProtocol.ProgressTrackSync playerName trackName trackData
        NetworkServer.broadcastMessage serverState msg Nothing
      NetworkState.ClientState clientState -> do
        let msg = NetworkProtocol.ProgressTrackSync playerName trackName trackData
        void $ NetworkClient.sendMessage clientState msg
      NetworkState.NoNetworkState -> return ()
