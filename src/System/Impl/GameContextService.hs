{-# LANGUAGE OverloadedStrings #-}
{- | Implementação do serviço GameContext usando armazenamento em JSON.
     
     Esta implementação salva os contextos em arquivos .slg no formato JSON,
     permitindo fácil leitura e modificação manual se necessário.
-}
module System.Impl.GameContextService (newHandle) where

import qualified System.GameContextContract as GameContext
import qualified System.ProgressContract as Progress
import qualified System.Constants as C
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (encode, decode)
import Control.Exception (try, IOException)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import System.Directory (doesFileExist, removeFile)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.List (partition)

-- | Handle interno para gerenciar estado mutável
newtype InternalHandle = InternalHandle
    { currentContext :: MVar (Maybe GameContext.Context)
    }

-- | Cria um novo handle para o serviço de contexto
newHandle :: IO GameContext.Handle
newHandle = do
    contextVar <- newMVar Nothing
    let iHandle = InternalHandle { currentContext = contextVar }
    
    return $ GameContext.Handle
        { GameContext.createContext = createContextImpl iHandle
        , GameContext.loadContext = loadContextImpl iHandle
        , GameContext.saveContext = saveContextImpl
        , GameContext.getCurrentContext = getCurrentContextImpl iHandle
        , GameContext.updateAttributes = updateAttributesImpl iHandle
        , GameContext.updateResources = updateResourcesImpl iHandle
        , GameContext.addWorldData = addWorldDataImpl iHandle
        , GameContext.addLogEntry = addLogEntryImpl iHandle
        , GameContext.getSessionLog = getSessionLogImpl
        , GameContext.clearSessionLog = clearSessionLogImpl iHandle
        , GameContext.addProgressTrack = addProgressTrackImpl iHandle
        , GameContext.updateProgressTrack = updateProgressTrackImpl iHandle
        , GameContext.getProgressTrack = getProgressTrackImpl
        , GameContext.removeProgressTrack = removeProgressTrackImpl iHandle
        , GameContext.addBonus = addBonusImpl iHandle
        , GameContext.removeBonus = removeBonusImpl iHandle
        , GameContext.getApplicableBonuses = getApplicableBonusesImpl
        , GameContext.consumeBonuses = consumeBonusesImpl iHandle
        , GameContext.clearBonuses = clearBonusesImpl iHandle
        , GameContext.addBond = addBondImpl iHandle
        , GameContext.removeBond = removeBondImpl iHandle
        , GameContext.hasBond = hasBondImpl
        , GameContext.listBonds = listBondsImpl
        , GameContext.deleteContext = deleteContextImpl
        }

-- | Gera o nome do arquivo para um personagem
getContextFileName :: T.Text -> FilePath
getContextFileName charName = T.unpack charName ++ C.configContextFileExtension C.defaultConfig

-- | Cria um novo contexto
createContextImpl :: InternalHandle -> T.Text -> GameContext.Attributes -> IO (Either GameContext.ContextError GameContext.Context)
createContextImpl iHandle charName attrs = do
    -- Valida nome do personagem
    let maxLen = C.configMaxCharacterNameLength C.defaultConfig
    if T.null charName || T.length charName > maxLen
        then return $ Left GameContext.InvalidCharacterName
        else do
            let fileName = getContextFileName charName
            exists <- doesFileExist fileName
            
            if exists
                then return $ Left GameContext.CharacterAlreadyExists
                else do
                    -- Cria recursos iniciais padrão
                    let cfg = C.defaultConfig
                    let defaultResources = GameContext.Resources
                            { GameContext.spirit = C.configDefaultResourceSpirit cfg
                            , GameContext.health = C.configDefaultResourceHealth cfg
                            , GameContext.supply = C.configDefaultResourceSupply cfg
                            , GameContext.momentum = C.configDefaultResourceMomentum cfg
                            , GameContext.experience = C.configDefaultResourceExperience cfg
                            }
                    
                    let character = GameContext.MainCharacter
                            { GameContext.name = charName
                            , GameContext.attributes = attrs
                            , GameContext.resources = defaultResources
                            }
                    
                    let context = GameContext.Context
                            { GameContext.mainCharacter = character
                            , GameContext.world = GameContext.World []
                            , GameContext.sessionLog = []
                            , GameContext.progressTracks = []
                            , GameContext.activeBonuses = []
                            , GameContext.bonds = []
                            }
                    
                    -- Salva contexto inicial
                    saveResult <- saveContextImpl context
                    case saveResult of
                        Left err -> return $ Left err
                        Right _ -> do
                            -- Atualiza contexto atual
                            modifyMVar (currentContext iHandle) $ \_ -> return (Just context, ())
                            return $ Right context

-- | Carrega contexto de arquivo
loadContextImpl :: InternalHandle -> T.Text -> IO (Either GameContext.ContextError GameContext.Context)
loadContextImpl iHandle charName = do
    let fileName = getContextFileName charName
    exists <- doesFileExist fileName
    
    if not exists
        then return $ Left (GameContext.FileError $ "Arquivo não encontrado: " ++ fileName)
        else do
            result <- try (BL.readFile fileName) :: IO (Either IOException BL.ByteString)
            case result of
                Left err -> return $ Left (GameContext.FileError $ show err)
                Right contents ->
                    case decode contents of
                        Nothing -> return $ Left (GameContext.ParseError "Erro ao parsear JSON")
                        Just ctx -> do
                            -- Atualiza contexto atual
                            modifyMVar (currentContext iHandle) $ \_ -> return (Just ctx, ())
                            return $ Right ctx

-- | Salva contexto em arquivo
saveContextImpl :: GameContext.Context -> IO (Either GameContext.ContextError FilePath)
saveContextImpl ctx = do
    let charName = GameContext.name . GameContext.mainCharacter $ ctx
    let fileName = getContextFileName charName
    let jsonContent = encode ctx
    
    result <- try (BL.writeFile fileName jsonContent) :: IO (Either IOException ())
    case result of
        Left err -> return $ Left (GameContext.FileError $ show err)
        Right _ -> return $ Right fileName

-- | Obtém contexto atual
getCurrentContextImpl :: InternalHandle -> IO (Maybe GameContext.Context)
getCurrentContextImpl iHandle = readMVar (currentContext iHandle)

-- | Atualiza atributos do personagem
updateAttributesImpl :: InternalHandle -> GameContext.Context -> GameContext.Attributes -> IO GameContext.Context
updateAttributesImpl iHandle ctx newAttrs = do
    let character = GameContext.mainCharacter ctx
    let updatedCharacter = character { GameContext.attributes = newAttrs }
    let updatedCtx = ctx { GameContext.mainCharacter = updatedCharacter }
    
    -- Atualiza contexto atual
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Atualiza recursos do personagem
updateResourcesImpl :: InternalHandle -> GameContext.Context -> GameContext.Resources -> IO GameContext.Context
updateResourcesImpl iHandle ctx newRes = do
    let character = GameContext.mainCharacter ctx
    let updatedCharacter = character { GameContext.resources = newRes }
    let updatedCtx = ctx { GameContext.mainCharacter = updatedCharacter }
    
    -- Atualiza contexto atual
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona dados ao mundo
addWorldDataImpl :: InternalHandle -> GameContext.Context -> T.Text -> IO GameContext.Context
addWorldDataImpl iHandle ctx newData = do
    let world = GameContext.world ctx
    let worldData = GameContext.wData world
    let updatedWorld = world { GameContext.wData = newData : worldData }
    let updatedCtx = ctx { GameContext.world = updatedWorld }
    
    -- Atualiza contexto atual
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona entrada ao log com timestamp
addLogEntryImpl :: InternalHandle -> GameContext.Context -> T.Text -> IO GameContext.Context
addLogEntryImpl iHandle ctx logEntry = do
    currentTime <- getCurrentTime
    let timeFormat = C.formatTimestamp C.gameConstants
    let timeStr = T.pack $ formatTime defaultTimeLocale timeFormat currentTime
    let timestampedEntry = C.formatLogEntry C.gameConstants timeStr logEntry
    let currentLog = GameContext.sessionLog ctx
    let updatedCtx = ctx { GameContext.sessionLog = timestampedEntry : currentLog }
    
    -- Atualiza contexto atual
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Obtém todos os logs do contexto (ordem cronológica reversa - mais recente primeiro)
getSessionLogImpl :: GameContext.Context -> [T.Text]
getSessionLogImpl = GameContext.sessionLog

-- | Limpa o log do contexto
clearSessionLogImpl :: InternalHandle -> GameContext.Context -> IO GameContext.Context
clearSessionLogImpl iHandle ctx = do
    let updatedCtx = ctx { GameContext.sessionLog = [] }
    
    -- Atualiza contexto atual
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona progress track ao contexto
addProgressTrackImpl :: InternalHandle -> GameContext.Context -> Progress.ProgressTrack -> IO GameContext.Context
addProgressTrackImpl iHandle ctx track = do
    let tracks = GameContext.progressTracks ctx
    let updatedCtx = ctx { GameContext.progressTracks = track : tracks }
    
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Atualiza progress track existente
updateProgressTrackImpl :: InternalHandle -> GameContext.Context -> T.Text -> Progress.ProgressTrack -> IO GameContext.Context
updateProgressTrackImpl iHandle ctx trackName updatedTrack = do
    let tracks = GameContext.progressTracks ctx
    let newTracks = map (\t -> if Progress.trackName t == trackName then updatedTrack else t) tracks
    let updatedCtx = ctx { GameContext.progressTracks = newTracks }
    
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Obtém progress track por nome
getProgressTrackImpl :: GameContext.Context -> T.Text -> Maybe Progress.ProgressTrack
getProgressTrackImpl ctx trackName =
    let tracks = GameContext.progressTracks ctx
    in findTrack tracks
  where
    findTrack [] = Nothing
    findTrack (t:ts)
      | Progress.trackName t == trackName = Just t
      | otherwise = findTrack ts

-- | Remove progress track
removeProgressTrackImpl :: InternalHandle -> GameContext.Context -> T.Text -> IO GameContext.Context
removeProgressTrackImpl iHandle ctx trackName = do
    let tracks = GameContext.progressTracks ctx
    let newTracks = filter (\t -> Progress.trackName t /= trackName) tracks
    let updatedCtx = ctx { GameContext.progressTracks = newTracks }
    
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona bônus ativo
addBonusImpl :: InternalHandle -> GameContext.Context -> GameContext.ActiveBonus -> IO GameContext.Context
addBonusImpl iHandle ctx bonus = do
    let bonuses = GameContext.activeBonuses ctx
    let updatedCtx = ctx { GameContext.activeBonuses = bonus : bonuses }
    
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Remove bônus específico
removeBonusImpl :: InternalHandle -> GameContext.Context -> GameContext.ActiveBonus -> IO GameContext.Context
removeBonusImpl iHandle ctx bonus = do
    let bonuses = GameContext.activeBonuses ctx
    let newBonuses = filter (/= bonus) bonuses
    let updatedCtx = ctx { GameContext.activeBonuses = newBonuses }
    
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Obtém bônus aplicáveis a um move
getApplicableBonusesImpl :: GameContext.Context -> Maybe T.Text -> [GameContext.ActiveBonus]
getApplicableBonusesImpl ctx maybeMoveName =
    let bonuses = GameContext.activeBonuses ctx
    in filter (isBonusApplicable maybeMoveName) bonuses
  where
    isBonusApplicable :: Maybe T.Text -> GameContext.ActiveBonus -> Bool
    isBonusApplicable _ bonus | GameContext.bonusType bonus == GameContext.Persistent = True
    isBonusApplicable _ bonus | GameContext.bonusType bonus == GameContext.NextRoll = True
    isBonusApplicable (Just moveName) bonus = 
      case GameContext.bonusType bonus of
        GameContext.NextMove targetMove -> moveName == targetMove
        _ -> False
    isBonusApplicable Nothing _ = False

-- | Consome bônus de uso único
consumeBonusesImpl :: InternalHandle -> GameContext.Context -> Maybe T.Text -> IO GameContext.Context
consumeBonusesImpl iHandle ctx maybeMoveName = do
    let bonuses = GameContext.activeBonuses ctx
    let (toConsume, toKeep) = partition (shouldConsume maybeMoveName) bonuses
    
    -- Mostra bônus consumidos
    mapM_ (\b -> putStrLn $ "  [Bônus consumido: " ++ T.unpack (GameContext.bonusDescription b) ++ " +" ++ show (GameContext.bonusValue b) ++ "]") toConsume
    
    let updatedCtx = ctx { GameContext.activeBonuses = toKeep }
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx
  where
    shouldConsume :: Maybe T.Text -> GameContext.ActiveBonus -> Bool
    shouldConsume _ bonus | GameContext.bonusType bonus == GameContext.NextRoll = True
    shouldConsume (Just moveName) bonus = 
      case GameContext.bonusType bonus of
        GameContext.NextMove targetMove -> moveName == targetMove
        _ -> False
    shouldConsume _ _ = False

-- | Limpa todos os bônus
clearBonusesImpl :: InternalHandle -> GameContext.Context -> IO GameContext.Context
clearBonusesImpl iHandle ctx = do
    let updatedCtx = ctx { GameContext.activeBonuses = [] }
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona bond
addBondImpl :: InternalHandle -> GameContext.Context -> GameContext.Bond -> IO GameContext.Context
addBondImpl iHandle ctx bond = do
    let bonds = GameContext.bonds ctx
    let updatedCtx = ctx { GameContext.bonds = bond : bonds }
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Remove bond por nome
removeBondImpl :: InternalHandle -> GameContext.Context -> T.Text -> IO GameContext.Context
removeBondImpl iHandle ctx bondName = do
    let bonds = GameContext.bonds ctx
    let newBonds = filter (\b -> GameContext.bondName b /= bondName) bonds
    let updatedCtx = ctx { GameContext.bonds = newBonds }
    modifyMVar (currentContext iHandle) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Verifica se tem bond com nome
hasBondImpl :: GameContext.Context -> T.Text -> Bool
hasBondImpl ctx bondName =
    any (\b -> GameContext.bondName b == bondName) (GameContext.bonds ctx)

-- | Lista todos os bonds
listBondsImpl :: GameContext.Context -> [GameContext.Bond]
listBondsImpl = GameContext.bonds

-- | Remove arquivo de contexto
deleteContextImpl :: T.Text -> IO (Either GameContext.ContextError ())
deleteContextImpl charName = do
    let fileName = getContextFileName charName
    exists <- doesFileExist fileName
    
    if not exists
        then return $ Left (GameContext.FileError $ "Arquivo não encontrado: " ++ fileName)
        else do
            result <- try (removeFile fileName) :: IO (Either IOException ())
            case result of
                Left err -> return $ Left (GameContext.FileError $ show err)
                Right _ -> return $ Right ()

