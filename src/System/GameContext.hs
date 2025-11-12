{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | GameContext Service - Gerencia o contexto do jogo incluindo personagem principal e mundo.
--
--     Este serviço é responsável por:
--     - Criar e gerenciar o personagem principal
--     - Salvar e carregar o contexto em arquivos JSON (formato: <nome_personagem>.json)
--     - Atualizar atributos e recursos do personagem
--     - Gerenciar o estado do mundo do jogo
--
--     Este módulo deve ser importado qualificado:
--     > import qualified System.GameContextContract as GameContext
module System.GameContext
  ( Context (..),
    MainCharacter (..),
    Attributes (..),
    Resources (..),
    World (..),
    ContextError (..),
    ActiveBonus (..),
    BonusType (..),
    Bond (..),
    BondType (..),
    BondCommandType (..),
    BondCommand (..),
    BondProcessingResponse (..),
    AssetType (..),
    AssetSkill (..),
    Asset (..),
    PlayerAsset (..),
    getCharacterName,
    isContextInitialized,
    createContext,
    loadContext,
    saveContext,
    getCurrentContext,
    getContextCache,
    updateAttributes,
    updateResources,
    addWorldData,
    addLogEntry,
    addSyncLogEntry,
    getSessionLog,
    clearSessionLog,
    addProgressTrack,
    updateProgressTrack,
    getProgressTrack,
    removeProgressTrack,
    addBonus,
    removeBonus,
    getApplicableBonuses,
    consumeBonuses,
    clearBonuses,
    addBond,
    removeBond,
    hasBond,
    listBonds,
    updateBond,
    deleteContext,
    processBondCommand,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (IOException, SomeException, try)
import Data.Aeson (FromJSON, ToJSON, decode, encode, parseJSON, withObject, (.:), (.:?))
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.List (partition)
import Data.Maybe (fromMaybe)
import qualified Data.Maybe
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified System.Constants as C
import System.Directory (doesFileExist, removeFile)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Progress as Progress
import qualified System.Util.SafeIO as SafeIO

data BonusType
  = NextRoll
  | NextMove T.Text
  | Persistent
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ActiveBonus = ActiveBonus
  { bonusType :: !BonusType,
    bonusValue :: !Int,
    bonusDescription :: !T.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BondType
  = PersonBond
  | CommunityBond
  | PlaceBond
  | Undefined
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Bond = Bond
  { bondName :: !T.Text,
    bondType :: !BondType,
    bondNotes :: !T.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data AssetType
  = Companion
  | Path
  | CombatTalent
  | Ritual
  | Module
  | SupportVehicle
  | CommandVehicle
  | Deed
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data AssetSkill = AssetSkill
  { skillIndex :: !Int,
    skillName :: !T.Text,
    skillDescription :: !T.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Asset = Asset
  { assetType :: !AssetType,
    assetName :: !T.Text,
    assetSkills :: ![AssetSkill]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PlayerAsset = PlayerAsset
  { playerAssetName :: !T.Text,
    enabledSkills :: ![Int]
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Attributes = Attributes
  { iron :: !Int,
    edge :: !Int,
    heart :: !Int,
    shadow :: !Int,
    wits :: !Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Resources = Resources
  { spirit :: !Int,
    health :: !Int,
    supply :: !Int,
    momentum :: !Int,
    experience :: !Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data MainCharacter = MainCharacter
  { name :: !T.Text,
    attributes :: !Attributes,
    resources :: !Resources,
    assets :: ![PlayerAsset]
  }
  deriving (Show, Eq, Generic, ToJSON)

newtype World = World
  { wData :: [T.Text]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Context = Context
  { mainCharacter :: !MainCharacter,
    world :: !World,
    sessionLog :: ![T.Text],
    progressTracks :: ![Progress.ProgressTrack],
    activeBonuses :: ![ActiveBonus],
    bonds :: ![Bond],
    isMultiplayer :: !Bool,
    multiplayerSessionId :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Generic, ToJSON)

instance FromJSON MainCharacter where
  parseJSON = withObject "MainCharacter" $ \v -> do
    n <- v .: "name"
    attrs <- v .: "attributes"
    res <- v .: "resources"
    maybeAssets <- v .:? "assets"
    let _assets = fromMaybe [] maybeAssets
    return $ MainCharacter n attrs res _assets

instance FromJSON Context where
  parseJSON = withObject "Context" $ \v -> do
    mc <- v .: "mainCharacter"
    w <- v .: "world"
    sl <- v .: "sessionLog"
    pt <- v .: "progressTracks"
    ab <- v .: "activeBonuses"
    maybeBonds <- v .:? "bonds"
    maybeMultiplayer <- v .:? "isMultiplayer"
    maybeSessionId <- v .:? "multiplayerSessionId"
    let _bonds = fromMaybe [] maybeBonds
    let _isMultiplayer = fromMaybe False maybeMultiplayer
    let _sessionId = maybeSessionId
    return $ Context mc w sl pt ab _bonds _isMultiplayer _sessionId

data ContextError
  = ContextNotInitialized
  | CharacterAlreadyExists
  | InvalidCommand
  | InvalidCharacterName
  | FileError String
  | ParseError String
  deriving (Show, Eq)

contextCacheRef :: MVar (Maybe (MVar (Maybe Context)))
contextCacheRef = unsafePerformIO (newMVar Nothing)
{-# NOINLINE contextCacheRef #-}

getContextCache :: IO (MVar (Maybe Context))
getContextCache = do
  maybeCache <- readMVar contextCacheRef
  case maybeCache of
    Just cache -> return cache
    Nothing -> do
      cache <- newMVar Nothing
      modifyMVar_ contextCacheRef (const (return (Just cache)))
      return cache

getCharacterName :: Context -> T.Text
getCharacterName = name . mainCharacter

isContextInitialized :: Maybe Context -> Bool
isContextInitialized = Data.Maybe.isJust

data BondCommandType = AddBond | AddPlaceBond | AddCommunityBond | RemoveBond | UpdateBondNotes T.Text | ListBonds | ListBondNotes T.Text

data BondCommand = BondCommand
  { bondCommandType :: BondCommandType,
    bond :: Bond
  }

data BondProcessingResponse = Response
  { bondProcessingResult :: Context,
    systemMessage :: T.Text
  }

safeModifyMVar :: MVar a -> (a -> IO (a, b)) -> IO (Either SomeException b)
safeModifyMVar mvar f = try $ modifyMVar mvar f

getContextFileName :: T.Text -> FilePath
getContextFileName charName = T.unpack charName ++ C.configContextFileExtension C.defaultConfig

createContext :: T.Text -> Attributes -> IO (Either ContextError Context)
createContext charName attrs = do
  let maxLen = C.configMaxCharacterNameLength C.defaultConfig
  if T.null charName || T.length charName > maxLen
    then return $ Left InvalidCharacterName
    else do
      let fileName = getContextFileName charName
      exists <- doesFileExist fileName

      if exists
        then return $ Left CharacterAlreadyExists
        else do
          let cfg = C.defaultConfig
          let defaultResources =
                Resources
                  { spirit = C.configDefaultResourceSpirit cfg,
                    health = C.configDefaultResourceHealth cfg,
                    supply = C.configDefaultResourceSupply cfg,
                    momentum = C.configDefaultResourceMomentum cfg,
                    experience = C.configDefaultResourceExperience cfg
                  }

          let character =
                MainCharacter
                  { name = charName,
                    attributes = attrs,
                    resources = defaultResources,
                    assets = []
                  }

          let context =
                Context
                  { mainCharacter = character,
                    world = World [],
                    sessionLog = [],
                    progressTracks = [],
                    activeBonuses = [],
                    bonds = [],
                    isMultiplayer = False,
                    multiplayerSessionId = Nothing
                  }

          saveResult <- saveContext context
          case saveResult of
            Left err -> return $ Left err
            Right _ -> do
              cache <- getContextCache
              modifyMVar cache $ \_ -> return (Just context, ())
              return $ Right context

loadContext :: T.Text -> IO (Either ContextError Context)
loadContext charName = do
  let fileName = getContextFileName charName
  exists <- doesFileExist fileName

  if not exists
    then return $ Left (FileError $ "Arquivo não encontrado: " ++ fileName)
    else
      SafeIO.safeReadFile fileName
        >>= either
          (return . Left . FileError . show)
          parseAndUpdateCache
  where
    parseAndUpdateCache :: BL.ByteString -> IO (Either ContextError Context)
    parseAndUpdateCache bytes =
      maybe
        (return $ Left (ParseError "Erro ao parsear JSON"))
        updateCache
        (decode bytes)

    updateCache :: Context -> IO (Either ContextError Context)
    updateCache ctx = do
      cache <- getContextCache
      safeModifyMVar cache (const $ return (Just ctx, ()))
        >>= either
          (return . Left . FileError . ("Erro ao atualizar contexto: " ++) . show)
          (const $ return $ Right ctx)

saveContext :: Context -> IO (Either ContextError FilePath)
saveContext ctx = do
  let charName = name . mainCharacter $ ctx
  let fileName = getContextFileName charName
  let jsonContent = encode ctx

  result <- SafeIO.safeWriteFile fileName jsonContent
  case result of
    Left err -> return $ Left (FileError $ show err)
    Right _ -> do
      cache <- getContextCache
      modifyMVar cache $ \_ -> return (Just ctx, ())
      return $ Right fileName

getCurrentContext :: IO (Maybe Context)
getCurrentContext = do
  cache <- getContextCache
  readMVar cache

updateAttributes :: Context -> Attributes -> IO Context
updateAttributes ctx newAttrs = do
  let character = mainCharacter ctx
  let updatedCharacter = character {attributes = newAttrs}
  let updatedCtx = ctx {mainCharacter = updatedCharacter}

  cache <- getContextCache
  modifyMVar cache $ \_ -> return (Just updatedCtx, ())
  return updatedCtx

updateResources :: Context -> Resources -> IO Context
updateResources ctx newRes = do
  let character = mainCharacter ctx
  let updatedCharacter = character {resources = newRes}
  let updatedCtx = ctx {mainCharacter = updatedCharacter}

  cache <- getContextCache
  modifyMVar cache $ \_ -> return (Just updatedCtx, ())
  return updatedCtx

addWorldData :: Context -> T.Text -> IO Context
addWorldData ctx newData = do
  let world' = world ctx
  let worldData = wData world'
  let updatedWorld = world' {wData = newData : worldData}
  let updatedCtx = ctx {world = updatedWorld}

  cache <- getContextCache
  modifyMVar cache $ \_ -> return (Just updatedCtx, ())
  return updatedCtx

addLogEntry :: Context -> T.Text -> IO Context
addLogEntry ctx logEntry = do
  let currentLog = sessionLog ctx
  let updatedCtx = ctx {sessionLog = logEntry : currentLog}

  cache <- getContextCache
  modifyMVar cache $ \_ -> return (Just updatedCtx, ())
  return updatedCtx

addSyncLogEntry :: Context -> T.Text -> IO Context
addSyncLogEntry = addLogEntry

getSessionLog :: Context -> [T.Text]
getSessionLog = sessionLog

clearSessionLog :: Context -> IO Context
clearSessionLog ctx = do
  let updatedCtx = ctx {sessionLog = []}

  cache <- getContextCache
  modifyMVar cache $ \_ -> return (Just updatedCtx, ())
  return updatedCtx

addProgressTrack :: Context -> Progress.ProgressTrack -> IO Context
addProgressTrack _ctx track = do
  cache <- getContextCache
  modifyMVar cache $ \maybeCtx -> do
    case maybeCtx of
      Nothing -> return (Just _ctx, _ctx)
      Just ctx -> do
        let tracks = progressTracks ctx
        let updatedCtx = ctx {progressTracks = track : tracks}
        return (Just updatedCtx, updatedCtx)

updateProgressTrack :: Context -> T.Text -> Progress.ProgressTrack -> IO Context
updateProgressTrack _ctx trackName updatedTrack = do
  cache <- getContextCache
  modifyMVar cache $ \maybeCtx -> do
    case maybeCtx of
      Nothing -> return (Just _ctx, _ctx)
      Just ctx -> do
        let tracks = progressTracks ctx
        let newTracks = map (\t -> if Progress.trackName t == trackName then updatedTrack else t) tracks
        let updatedCtx = ctx {progressTracks = newTracks}
        return (Just updatedCtx, updatedCtx)

getProgressTrack :: Context -> T.Text -> Maybe Progress.ProgressTrack
getProgressTrack ctx trackName =
  let tracks = progressTracks ctx
   in findTrack tracks
  where
    findTrack [] = Nothing
    findTrack (t : ts)
      | Progress.trackName t == trackName = Just t
      | otherwise = findTrack ts

removeProgressTrack :: Context -> T.Text -> IO Context
removeProgressTrack ctx trackName = do
  let tracks = progressTracks ctx
  let newTracks = filter (\t -> Progress.trackName t /= trackName) tracks
  let updatedCtx = ctx {progressTracks = newTracks}

  cache <- getContextCache
  modifyMVar cache $ \_ -> return (Just updatedCtx, ())
  return updatedCtx

addBonus :: Context -> ActiveBonus -> IO Context
addBonus ctx bonus = do
  let bonuses = activeBonuses ctx
  let updatedCtx = ctx {activeBonuses = bonus : bonuses}

  cache <- getContextCache
  modifyMVar cache $ \_ -> return (Just updatedCtx, ())
  return updatedCtx

removeBonus :: Context -> ActiveBonus -> IO Context
removeBonus ctx bonus = do
  let bonuses = activeBonuses ctx
  let newBonuses = filter (/= bonus) bonuses
  let updatedCtx = ctx {activeBonuses = newBonuses}

  cache <- getContextCache
  modifyMVar cache $ \_ -> return (Just updatedCtx, ())
  return updatedCtx

getApplicableBonuses :: Context -> Maybe T.Text -> [ActiveBonus]
getApplicableBonuses ctx maybeMoveName =
  let bonuses = activeBonuses ctx
   in filter (isBonusApplicable maybeMoveName) bonuses
  where
    isBonusApplicable :: Maybe T.Text -> ActiveBonus -> Bool
    isBonusApplicable _ bonus | bonusType bonus == Persistent = True
    isBonusApplicable _ bonus | bonusType bonus == NextRoll = True
    isBonusApplicable (Just moveName) bonus =
      case bonusType bonus of
        NextMove targetMove -> moveName == targetMove
        _ -> False
    isBonusApplicable Nothing _ = False

consumeBonuses :: Context -> Maybe T.Text -> IO Context
consumeBonuses ctx maybeMoveName = do
  let bonuses = activeBonuses ctx
  let (toConsume, toKeep) = partition (shouldConsume maybeMoveName) bonuses

  mapM_ (\b -> putStrLn $ "  [Bônus consumido: " ++ T.unpack (bonusDescription b) ++ " +" ++ show (bonusValue b) ++ "]") toConsume

  let updatedCtx = ctx {activeBonuses = toKeep}
  cache <- getContextCache
  modifyMVar cache $ \_ -> return (Just updatedCtx, ())
  return updatedCtx
  where
    shouldConsume :: Maybe T.Text -> ActiveBonus -> Bool
    shouldConsume _ bonus | bonusType bonus == NextRoll = True
    shouldConsume (Just moveName) bonus =
      case bonusType bonus of
        NextMove targetMove -> moveName == targetMove
        _ -> False
    shouldConsume _ _ = False

clearBonuses :: Context -> IO Context
clearBonuses ctx = do
  let updatedCtx = ctx {activeBonuses = []}
  cache <- getContextCache
  modifyMVar cache $ \_ -> return (Just updatedCtx, ())
  return updatedCtx

hasBond :: Context -> T.Text -> Bool
hasBond ctx bn =
  any (\b -> bondName b == bn) (bonds ctx)

listBonds :: Context -> IO (Either ContextError BondProcessingResponse)
listBonds ctx = return successResponse
  where
    successResponse = Right $ Response {bondProcessingResult = ctx, systemMessage = T.unlines (ctx & bonds & map bondName)}

deleteContext :: T.Text -> IO (Either ContextError ())
deleteContext charName = do
  let fileName = getContextFileName charName
  exists <- doesFileExist fileName

  if not exists
    then return $ Left (FileError $ "Arquivo não encontrado: " ++ fileName)
    else do
      result <- try (removeFile fileName) :: IO (Either IOException ())
      case result of
        Left err -> return $ Left (FileError $ show err)
        Right _ -> return $ Right ()

addBond :: Context -> Bond -> IO (Either ContextError BondProcessingResponse)
addBond _ctx b = do
  cache <- getContextCache
  modifyMVar cache $ \maybeCtx -> do
    case maybeCtx of
      Nothing -> return (Just _ctx, errorResponse)
      Just ctx -> do
        let bonds' = bonds ctx
        let updatedCtx = ctx {bonds = b : bonds'}
        return (Just updatedCtx, successResponse updatedCtx)
  where
    successResponse ctx = Right $ Response {bondProcessingResult = ctx, systemMessage = "Bond added successfully"}
    errorResponse = Left $ FileError "Context not found"

updateBond :: Context -> T.Text -> Bond -> IO (Either ContextError BondProcessingResponse)
updateBond _ctx bondNameToUpdate updateToApply = do
  cache <- getContextCache
  modifyMVar cache $ \maybeCtx -> do
    case maybeCtx of
      Nothing -> return (Just _ctx, errorResponse)
      Just ctx -> do
        let bonds' = bonds ctx
        if not (hasBond ctx bondNameToUpdate)
          then return (Just ctx, bondNotFoundError)
          else do
            let newNote = updateToApply & bondNotes
            let newBonds =
                  map
                    ( \b ->
                        if bondName b == bondNameToUpdate
                          then
                            let appendedNotes = appendNote (bondNotes b) newNote
                             in b {bondNotes = appendedNotes}
                          else b
                    )
                    bonds'
            let noteMessage = "~{ " <> bondNameToUpdate <> " - " <> newNote <> " }~"
            let updatedCtx =
                  ctx
                    { bonds = newBonds,
                      sessionLog = noteMessage : sessionLog ctx
                    }
            saveResult <- saveContext updatedCtx
            case saveResult of
              Left err -> return (Just ctx, Left err)
              Right _ -> return (Just updatedCtx, successResponse updatedCtx noteMessage)
  where
    successResponse ctx noteMessage = Right $ Response {bondProcessingResult = ctx, systemMessage = noteMessage}
    errorResponse = Left $ FileError "Context not found"
    bondNotFoundError = Left $ FileError "Bond not found"

    appendNote :: T.Text -> T.Text -> T.Text
    appendNote existing newNote
      | T.null newNote = existing
      | T.null existing = newNote
      | otherwise = existing <> "\n---\n" <> newNote

removeBond :: Context -> T.Text -> IO (Either ContextError BondProcessingResponse)
removeBond _ctx bn = do
  cache <- getContextCache
  modifyMVar cache $ \maybeCtx -> do
    case maybeCtx of
      Nothing -> return (Just _ctx, errorResponse)
      Just ctx -> do
        let bonds' = bonds ctx
        let newBonds = filter (\b -> bondName b /= bn) bonds'
        let updatedCtx = ctx {bonds = newBonds}
        return (Just updatedCtx, successResponse updatedCtx)
  where
    successResponse ctx = Right $ Response {bondProcessingResult = ctx, systemMessage = "Bond removed successfully"}
    errorResponse = Left $ FileError "Context not found"

listBondNotes :: Context -> T.Text -> IO (Either ContextError BondProcessingResponse)
listBondNotes _ctx bondNameParam = return successResponse
  where
    successResponse =
      Right $
        Response
          { bondProcessingResult = _ctx,
            systemMessage =
              T.unlines (_ctx & bonds & filter (\b -> bondName b == bondNameParam) & map bondNotes)
          }

processBondCommand :: BondCommand -> IO (Either ContextError BondProcessingResponse)
processBondCommand BondCommand {bondCommandType = cmdType, bond = param} = do
  cache <- getContextCache
  maybeCtx <- readMVar cache
  exec maybeCtx
  where
    exec Nothing = return (Left $ FileError "Context not loaded")
    exec (Just ctx) = case cmdType of
      AddBond -> addBond ctx param
      ListBonds -> listBonds ctx
      AddPlaceBond -> addBond ctx param
      AddCommunityBond -> addBond ctx param
      RemoveBond -> removeBond ctx (param & bondName)
      UpdateBondNotes bondNameParam -> updateBond ctx bondNameParam param
      ListBondNotes bondNameParam -> listBondNotes ctx bondNameParam