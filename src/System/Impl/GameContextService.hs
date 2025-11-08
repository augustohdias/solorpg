{-# LANGUAGE OverloadedStrings #-}
{- | Implementação do serviço GameContext usando armazenamento em JSON.
     
     Esta implementação salva os contextos em arquivos .json no formato JSON,
     permitindo fácil leitura e modificação manual se necessário.
-}
module System.Impl.GameContextService (newHandle) where

import qualified System.GameContextContract as G
import qualified System.ProgressContract as Progress
import qualified System.Constants as C
import qualified Data.Text as T
import Data.Aeson (encode, decode)
import Control.Exception (try, IOException, SomeException)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import System.Directory (doesFileExist, removeFile)
import qualified System.Util.SafeIO as SafeIO
import Data.List (partition)
import System.GameContextContract (BondCommand (BondCommand, bondCommandType, bond), Context, ContextError, BondProcessingResponse (Response, bondProcessingResult, systemMessage), BondCommandType (..), Bond (bondName, bondNotes))
import Data.Function ((&))

-- | Handle interno para gerenciar estado mutável
newtype Service = Service
    { currentContext :: MVar (Maybe G.Context)
    }

-- | Safe wrapper for modifyMVar that handles exceptions
-- Prevents deadlock if an exception occurs during modification
safeModifyMVar :: MVar a -> (a -> IO (a, b)) -> IO (Either SomeException b)
safeModifyMVar mvar f =
    try $ modifyMVar mvar f


-- | Cria um novo handle para o serviço de contexto
newHandle :: IO G.Handle
newHandle = do
    contextVar <- newMVar Nothing
    let service = Service { currentContext = contextVar }

    return $ G.Handle
        { G.createContext = createContextImpl service
        , G.loadContext = loadContextImpl service
        , G.saveContext = saveContextImpl
        , G.getCurrentContext = getCurrentContextImpl service
        , G.updateAttributes = updateAttributesImpl service
        , G.updateResources = updateResourcesImpl service
        , G.addWorldData = addWorldDataImpl service
        , G.addLogEntry = addLogEntryImpl service
        , G.getSessionLog = getSessionLogImpl
        , G.clearSessionLog = clearSessionLogImpl service
        , G.addProgressTrack = addProgressTrackImpl service
        , G.updateProgressTrack = updateProgressTrackImpl service
        , G.getProgressTrack = getProgressTrackImpl
        , G.removeProgressTrack = removeProgressTrackImpl service
        , G.addBonus = addBonusImpl service
        , G.removeBonus = removeBonusImpl service
        , G.getApplicableBonuses = getApplicableBonusesImpl
        , G.consumeBonuses = consumeBonusesImpl service
        , G.clearBonuses = clearBonusesImpl service
        , G.addBond = addBondImpl service
        , G.removeBond = removeBondImpl service
        , G.hasBond = hasBondImpl
        , G.listBonds = listBonds
        , G.updateBond = updateBondImpl service
        , G.deleteContext = deleteContextImpl
        , G.processBondCommand = processBondCommand service
        }

-- | Gera o nome do arquivo para um personagem
getContextFileName :: T.Text -> FilePath
getContextFileName charName = T.unpack charName ++ C.configContextFileExtension C.defaultConfig

-- | Cria um novo contexto
createContextImpl :: Service -> T.Text -> G.Attributes -> IO (Either G.ContextError G.Context)
createContextImpl service charName attrs = do
    -- Valida nome do personagem
    let maxLen = C.configMaxCharacterNameLength C.defaultConfig
    if T.null charName || T.length charName > maxLen
        then return $ Left G.InvalidCharacterName
        else do
            let fileName = getContextFileName charName
            exists <- doesFileExist fileName

            if exists
                then return $ Left G.CharacterAlreadyExists
                else do
                    -- Cria recursos iniciais padrão
                    let cfg = C.defaultConfig
                    let defaultResources = G.Resources
                            { G.spirit = C.configDefaultResourceSpirit cfg
                            , G.health = C.configDefaultResourceHealth cfg
                            , G.supply = C.configDefaultResourceSupply cfg
                            , G.momentum = C.configDefaultResourceMomentum cfg
                            , G.experience = C.configDefaultResourceExperience cfg
                            }

                    let character = G.MainCharacter
                            { G.name = charName
                            , G.attributes = attrs
                            , G.resources = defaultResources
                            }

                    let context = G.Context
                            { G.mainCharacter = character
                            , G.world = G.World []
                            , G.sessionLog = []
                            , G.progressTracks = []
                            , G.activeBonuses = []
                            , G.bonds = []
                            }

                    -- Salva contexto inicial
                    saveResult <- saveContextImpl context
                    case saveResult of
                        Left err -> return $ Left err
                        Right _ -> do
                            -- Atualiza contexto atual
                            modifyMVar (currentContext service) $ \_ -> return (Just context, ())
                            return $ Right context

-- | Carrega contexto de arquivo (thread-safe)
loadContextImpl :: Service -> T.Text -> IO (Either G.ContextError G.Context)
loadContextImpl service charName = do
    let fileName = getContextFileName charName
    exists <- doesFileExist fileName

    if not exists
        then return $ Left (G.FileError $ "Arquivo não encontrado: " ++ fileName)
        else do
            -- Use safe file read to prevent race conditions
            result <- SafeIO.safeReadFile fileName
            case result of
                Left err -> return $ Left (G.FileError $ show err)
                Right contents ->
                    case decode contents of
                        Nothing -> return $ Left (G.ParseError "Erro ao parsear JSON")
                        Just ctx -> do
                            -- Atualiza contexto atual (thread-safe)
                            modifyResult <- safeModifyMVar (currentContext service) $ \_ ->
                                return (Just ctx, ())
                            case modifyResult of
                                Left err -> return $ Left (G.FileError $ "Erro ao atualizar contexto: " ++ show err)
                                Right _ -> return $ Right ctx

-- | Salva contexto em arquivo (thread-safe)
saveContextImpl :: G.Context -> IO (Either G.ContextError FilePath)
saveContextImpl ctx = do
    let charName = G.name . G.mainCharacter $ ctx
    let fileName = getContextFileName charName
    let jsonContent = encode ctx

    -- Use safe file write to prevent concurrent writes
    result <- SafeIO.safeWriteFile fileName jsonContent
    case result of
        Left err -> return $ Left (G.FileError $ show err)
        Right _ -> return $ Right fileName

-- | Obtém contexto atual
getCurrentContextImpl :: Service -> IO (Maybe G.Context)
getCurrentContextImpl service = readMVar (currentContext service)

-- | Atualiza atributos do personagem
updateAttributesImpl :: Service -> G.Context -> G.Attributes -> IO G.Context
updateAttributesImpl service ctx newAttrs = do
    let character = G.mainCharacter ctx
    let updatedCharacter = character { G.attributes = newAttrs }
    let updatedCtx = ctx { G.mainCharacter = updatedCharacter }

    -- Atualiza contexto atual
    modifyMVar (currentContext service) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Atualiza recursos do personagem
updateResourcesImpl :: Service -> G.Context -> G.Resources -> IO G.Context
updateResourcesImpl service ctx newRes = do
    let character = G.mainCharacter ctx
    let updatedCharacter = character { G.resources = newRes }
    let updatedCtx = ctx { G.mainCharacter = updatedCharacter }

    -- Atualiza contexto atual
    modifyMVar (currentContext service) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona dados ao mundo
addWorldDataImpl :: Service -> G.Context -> T.Text -> IO G.Context
addWorldDataImpl service ctx newData = do
    let world = G.world ctx
    let worldData = G.wData world
    let updatedWorld = world { G.wData = newData : worldData }
    let updatedCtx = ctx { G.world = updatedWorld }

    -- Atualiza contexto atual
    modifyMVar (currentContext service) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona entrada ao log com timestamp
addLogEntryImpl :: Service -> G.Context -> T.Text -> IO G.Context
addLogEntryImpl service ctx logEntry = do
    let currentLog = G.sessionLog ctx
    let updatedCtx = ctx { G.sessionLog = logEntry : currentLog }

    -- Atualiza contexto atual
    modifyMVar (currentContext service) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Obtém todos os logs do contexto (ordem cronológica reversa - mais recente primeiro)
getSessionLogImpl :: G.Context -> [T.Text]
getSessionLogImpl = G.sessionLog

-- | Limpa o log do contexto
clearSessionLogImpl :: Service -> G.Context -> IO G.Context
clearSessionLogImpl service ctx = do
    let updatedCtx = ctx { G.sessionLog = [] }

    -- Atualiza contexto atual
    modifyMVar (currentContext service) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona progress track ao contexto
addProgressTrackImpl :: Service -> G.Context -> Progress.ProgressTrack -> IO G.Context
addProgressTrackImpl service _ctx track = do
    -- Use modifyMVar atomically to read current context and update it
    modifyMVar (currentContext service) $ \maybeCtx -> do
        case maybeCtx of
            Nothing -> return (Just _ctx, _ctx)  -- Fallback to passed context if MVar is empty
            Just ctx -> do
                let tracks = G.progressTracks ctx
                let updatedCtx = ctx { G.progressTracks = track : tracks }
                return (Just updatedCtx, updatedCtx)

-- | Atualiza progress track existente
updateProgressTrackImpl :: Service -> G.Context -> T.Text -> Progress.ProgressTrack -> IO G.Context
updateProgressTrackImpl service _ctx trackName updatedTrack = do
    -- Use modifyMVar atomically to read current context and update it
    modifyMVar (currentContext service) $ \maybeCtx -> do
        case maybeCtx of
            Nothing -> return (Just _ctx, _ctx)  -- Fallback to passed context if MVar is empty
            Just ctx -> do
                let tracks = G.progressTracks ctx
                let newTracks = map (\t -> if Progress.trackName t == trackName then updatedTrack else t) tracks
                let updatedCtx = ctx { G.progressTracks = newTracks }
                return (Just updatedCtx, updatedCtx)

-- | Obtém progress track por nome
getProgressTrackImpl :: G.Context -> T.Text -> Maybe Progress.ProgressTrack
getProgressTrackImpl ctx trackName =
    let tracks = G.progressTracks ctx
    in findTrack tracks
  where
    findTrack [] = Nothing
    findTrack (t:ts)
      | Progress.trackName t == trackName = Just t
      | otherwise = findTrack ts

-- | Remove progress track
removeProgressTrackImpl :: Service -> G.Context -> T.Text -> IO G.Context
removeProgressTrackImpl service ctx trackName = do
    let tracks = G.progressTracks ctx
    let newTracks = filter (\t -> Progress.trackName t /= trackName) tracks
    let updatedCtx = ctx { G.progressTracks = newTracks }

    modifyMVar (currentContext service) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona bônus ativo
addBonusImpl :: Service -> G.Context -> G.ActiveBonus -> IO G.Context
addBonusImpl service ctx bonus = do
    let bonuses = G.activeBonuses ctx
    let updatedCtx = ctx { G.activeBonuses = bonus : bonuses }

    modifyMVar (currentContext service) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Remove bônus específico
removeBonusImpl :: Service -> G.Context -> G.ActiveBonus -> IO G.Context
removeBonusImpl service ctx bonus = do
    let bonuses = G.activeBonuses ctx
    let newBonuses = filter (/= bonus) bonuses
    let updatedCtx = ctx { G.activeBonuses = newBonuses }

    modifyMVar (currentContext service) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Obtém bônus aplicáveis a um move
getApplicableBonusesImpl :: G.Context -> Maybe T.Text -> [G.ActiveBonus]
getApplicableBonusesImpl ctx maybeMoveName =
    let bonuses = G.activeBonuses ctx
    in filter (isBonusApplicable maybeMoveName) bonuses
  where
    isBonusApplicable :: Maybe T.Text -> G.ActiveBonus -> Bool
    isBonusApplicable _ bonus | G.bonusType bonus == G.Persistent = True
    isBonusApplicable _ bonus | G.bonusType bonus == G.NextRoll = True
    isBonusApplicable (Just moveName) bonus =
      case G.bonusType bonus of
        G.NextMove targetMove -> moveName == targetMove
        _ -> False
    isBonusApplicable Nothing _ = False

-- | Consome bônus de uso único
consumeBonusesImpl :: Service -> G.Context -> Maybe T.Text -> IO G.Context
consumeBonusesImpl service ctx maybeMoveName = do
    let bonuses = G.activeBonuses ctx
    let (toConsume, toKeep) = partition (shouldConsume maybeMoveName) bonuses

    -- Mostra bônus consumidos
    mapM_ (\b -> putStrLn $ "  [Bônus consumido: " ++ T.unpack (G.bonusDescription b) ++ " +" ++ show (G.bonusValue b) ++ "]") toConsume

    let updatedCtx = ctx { G.activeBonuses = toKeep }
    modifyMVar (currentContext service) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx
  where
    shouldConsume :: Maybe T.Text -> G.ActiveBonus -> Bool
    shouldConsume _ bonus | G.bonusType bonus == G.NextRoll = True
    shouldConsume (Just moveName) bonus =
      case G.bonusType bonus of
        G.NextMove targetMove -> moveName == targetMove
        _ -> False
    shouldConsume _ _ = False

-- | Limpa todos os bônus
clearBonusesImpl :: Service -> G.Context -> IO G.Context
clearBonusesImpl service ctx = do
    let updatedCtx = ctx { G.activeBonuses = [] }
    modifyMVar (currentContext service) $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Verifica se tem bond com nome
hasBondImpl :: G.Context -> T.Text -> Bool
hasBondImpl ctx bn =
    any (\b -> G.bondName b == bn) (G.bonds ctx)

listBonds :: Context -> IO (Either ContextError BondProcessingResponse)
listBonds ctx = return successResponse where
    successResponse = Right $ Response { bondProcessingResult = ctx, systemMessage = T.unlines (ctx&G.bonds&map G.bondName) }

-- | Remove arquivo de contexto
deleteContextImpl :: T.Text -> IO (Either G.ContextError ())
deleteContextImpl charName = do
    let fileName = getContextFileName charName
    exists <- doesFileExist fileName

    if not exists
        then return $ Left (G.FileError $ "Arquivo não encontrado: " ++ fileName)
        else do
            result <- try (removeFile fileName) :: IO (Either IOException ())
            case result of
                Left err -> return $ Left (G.FileError $ show err)
                Right _ -> return $ Right ()

addBondImpl :: Service -> Context -> Bond -> IO (Either ContextError BondProcessingResponse)
addBondImpl service _ctx b = modifyMVar (currentContext service) update where
    update Nothing = return (Just _ctx, errorResponse) -- Fallback to passed context if MVar is empty
    update (Just ctx) = do
        let bonds = G.bonds ctx
        let updatedCtx = ctx { G.bonds = b : bonds }
        return (Just updatedCtx, successResponse updatedCtx)
    successResponse ctx = Right $ Response { bondProcessingResult = ctx, systemMessage = "Bond added successfully" }
    errorResponse = Left $ G.FileError "Context not found"

updateBondImpl :: Service -> Context -> T.Text -> Bond -> IO (Either ContextError BondProcessingResponse)
updateBondImpl service _ctx name updateToApply = modifyMVar (service&currentContext) update where
    update Nothing = return (Just _ctx, errorResponse) -- Fallback to passed context if MVar is empty
    update (Just ctx) = do
        let bonds = G.bonds ctx
        if not (hasBondImpl ctx name)
            then return (Just ctx, bondNotFoundError)
            else do
                let newNote = updateToApply & bondNotes
                let newBonds =
                        map
                            ( \b ->
                                if G.bondName b == name
                                    then
                                        let appendedNotes = appendNote (G.bondNotes b) newNote
                                        in b { G.bondNotes = appendedNotes }
                                    else b
                            )
                            bonds
                let noteMessage = "~{ " <> name <> " - " <> newNote <> " }~"
                let updatedCtx =
                        ctx
                            { G.bonds = newBonds
                            , G.sessionLog = noteMessage : G.sessionLog ctx
                            }
                saveResult <- saveContextImpl updatedCtx
                case saveResult of
                    Left err -> return (Just ctx, Left err)
                    Right _ -> return (Just updatedCtx, successResponse updatedCtx noteMessage)
    successResponse ctx noteMessage = Right $ Response { bondProcessingResult = ctx, systemMessage = noteMessage }
    errorResponse = Left $ G.FileError "Context not found"
    bondNotFoundError = Left $ G.FileError "Bond not found"

    appendNote :: T.Text -> T.Text -> T.Text
    appendNote existing newNote
        | T.null newNote = existing
        | T.null existing = newNote
        | otherwise = existing <> "\n---\n" <> newNote

removeBondImpl :: Service -> Context -> T.Text -> IO (Either ContextError BondProcessingResponse)
removeBondImpl service _ctx bn = modifyMVar (currentContext service) update where
    update Nothing = return (Just _ctx, errorResponse)
    update (Just ctx) = do
        let bonds = G.bonds ctx
        let newBonds = filter (\b -> G.bondName b /= bn) bonds
        let updatedCtx = ctx { G.bonds = newBonds }
        return (Just updatedCtx, successResponse updatedCtx)
    successResponse ctx = Right $ Response { bondProcessingResult = ctx, systemMessage = "Bond removed successfully" }
    errorResponse = Left $ G.FileError "Context not found"

listBondNotesImpl :: Service -> Context -> T.Text -> IO (Either ContextError BondProcessingResponse)
listBondNotesImpl _ _ctx name = return successResponse where
    successResponse = Right $ Response { bondProcessingResult = _ctx, systemMessage = 
        T.unlines (_ctx&G.bonds&filter (\b -> G.bondName b == name)&map G.bondNotes) }

processBondCommand :: Service -> BondCommand -> IO (Either ContextError BondProcessingResponse)
processBondCommand service BondCommand { bondCommandType = cmdType, bond = param } = do
    maybeCtx <- service&currentContext&readMVar
    exec maybeCtx where
    exec Nothing = return (Left $ G.FileError "Context not loaded")
    exec (Just ctx) = case cmdType of
        AddBond -> addBondImpl service ctx param
        ListBonds -> listBonds ctx
        AddPlaceBond -> addBondImpl service ctx param
        AddCommunityBond -> addBondImpl service ctx param
        RemoveBond -> removeBondImpl service ctx (param&bondName)
        UpdateBondNotes name -> updateBondImpl service ctx name param
        ListBondNotes name -> listBondNotesImpl service ctx name