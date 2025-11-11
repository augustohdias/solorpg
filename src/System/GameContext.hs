{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{- | GameContext Service - Gerencia o contexto do jogo incluindo personagem principal e mundo.
     
     Este serviço é responsável por:
     - Criar e gerenciar o personagem principal
     - Salvar e carregar o contexto em arquivos JSON (formato: <nome_personagem>.json)
     - Atualizar atributos e recursos do personagem
     - Gerenciar o estado do mundo do jogo
     
     Este módulo deve ser importado qualificado:
     > import qualified System.GameContextContract as GameContext
-}
module System.GameContext
  ( -- * Pure types
    Context (..)
  , MainCharacter (..)
  , Attributes (..)
  , Resources (..)
  , World (..)
  , ContextError (..)
  , ActiveBonus (..)
  , BonusType (..)
  , Bond (..)
  , BondType (..)
  , BondCommandType (..)
  , BondCommand (..)
  , BondProcessingResponse (..)

    -- * Derived functions
  , getCharacterName
  , isContextInitialized
    
    -- * Context operations
  , createContext
  , loadContext
  , saveContext
  , getCurrentContext
  , updateAttributes
  , updateResources
  , addWorldData
  , addLogEntry
  , getSessionLog
  , clearSessionLog
  , addProgressTrack
  , updateProgressTrack
  , getProgressTrack
  , removeProgressTrack
  , addBonus
  , removeBonus
  , getApplicableBonuses
  , consumeBonuses
  , clearBonuses
  , addBond
  , removeBond
  , hasBond
  , listBonds
  , updateBond
  , deleteContext
  , processBondCommand
  ) where

import qualified System.Progress as Progress
import qualified System.Constants as C
import qualified System.Util.SafeIO as SafeIO
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, parseJSON, withObject, (.:), (.:?), encode, decode)
import Data.Maybe (fromMaybe)
import qualified Data.Maybe
import Control.Exception (try, IOException, SomeException)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import System.Directory (doesFileExist, removeFile)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (partition)
import Data.Function ((&))

-- | Tipo de bônus
data BonusType
  = NextRoll        -- ^ Próxima rolagem qualquer
  | NextMove T.Text -- ^ Próximo move específico (nome do move)
  | Persistent      -- ^ Bônus permanente (até removido manualmente)
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Bônus ativo no contexto
data ActiveBonus = ActiveBonus
  { bonusType :: !BonusType
  , bonusValue :: !Int
  , bonusDescription :: !T.Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Tipo de vínculo (bond)
data BondType
  = PersonBond      -- ^ Vínculo com pessoa
  | CommunityBond   -- ^ Vínculo com comunidade
  | PlaceBond       -- ^ Vínculo com lugar
  | Undefined
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Vínculo com pessoa, comunidade ou lugar
data Bond = Bond
  { bondName :: !T.Text       -- ^ Nome da pessoa/comunidade/lugar
  , bondType :: !BondType     -- ^ Tipo do vínculo
  , bondNotes :: !T.Text      -- ^ Notas sobre o vínculo
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Atributos do personagem principal (sistema Ironsworn)
data Attributes = Attributes
    { iron :: !Int      -- ^ Força física e resistência
    , edge :: !Int      -- ^ Velocidade, agilidade e precisão
    , heart :: !Int     -- ^ Coragem, empatia e charme
    , shadow :: !Int    -- ^ Astúcia, furtividade e engano
    , wits :: !Int      -- ^ Perícia, conhecimento e observação
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Recursos do personagem principal
data Resources = Resources
    { spirit :: !Int      -- ^ Saúde mental
    , health :: !Int      -- ^ Saúde física
    , supply :: !Int      -- ^ Suprimentos
    , momentum :: !Int    -- ^ Impulso narrativo
    , experience :: !Int  -- ^ Experiência acumulada
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Personagem principal
data MainCharacter = MainCharacter
    { name :: !T.Text
    , attributes :: !Attributes
    , resources :: !Resources
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Estado do mundo do jogo
newtype World = World
    { wData :: [T.Text]  -- ^ Dados narrativos do mundo
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Contexto completo do jogo
data Context = Context
    { mainCharacter :: !MainCharacter
    , world :: !World
    , sessionLog :: ![T.Text]           -- ^ Histórico de eventos do jogo
    , progressTracks :: ![Progress.ProgressTrack]  -- ^ Tracks ativos (vows, combats, journeys)
    , activeBonuses :: ![ActiveBonus]   -- ^ Bônus ativos temporários
    , bonds :: ![Bond]                  -- ^ Vínculos com pessoas, comunidades e lugares
    } deriving (Show, Eq, Generic, ToJSON)

-- | Instância customizada de FromJSON para Context
-- Garante que o campo 'bonds' seja inicializado como lista vazia se não existir no JSON
-- (compatibilidade com arquivos antigos criados antes da implementação de bonds)
instance FromJSON Context where
  parseJSON = withObject "Context" $ \v -> do
    mc <- v .: "mainCharacter"
    w <- v .: "world"
    sl <- v .: "sessionLog"
    pt <- v .: "progressTracks"
    ab <- v .: "activeBonuses"
    maybeBonds <- v .:? "bonds"
    let _bonds = fromMaybe [] maybeBonds
    return $ Context mc w sl pt ab _bonds

-- | Erros possíveis ao manipular contexto
data ContextError
    = ContextNotInitialized       -- ^ Contexto não foi inicializado
    | CharacterAlreadyExists      -- ^ Personagem já existe
    | InvalidCommand
    | InvalidCharacterName        -- ^ Nome de personagem inválido
    | FileError String            -- ^ Erro ao manipular arquivo
    | ParseError String           -- ^ Erro ao parsear JSON
    deriving (Show, Eq)

-- | Cache global do contexto atual (usando MVar para thread-safety)
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

-- | Obtém o nome do personagem principal do contexto
getCharacterName :: Context -> T.Text
getCharacterName = name . mainCharacter

-- | Verifica se o contexto está inicializado
isContextInitialized :: Maybe Context -> Bool
isContextInitialized = Data.Maybe.isJust


data BondCommandType = AddBond | AddPlaceBond | AddCommunityBond | RemoveBond | UpdateBondNotes T.Text | ListBonds | ListBondNotes T.Text

data BondCommand = BondCommand
  { bondCommandType :: BondCommandType
  , bond :: Bond
  }

data BondProcessingResponse = Response
  { bondProcessingResult :: Context
  , systemMessage :: T.Text
  }

-- | Safe wrapper for modifyMVar that handles exceptions
safeModifyMVar :: MVar a -> (a -> IO (a, b)) -> IO (Either SomeException b)
safeModifyMVar mvar f = try $ modifyMVar mvar f

-- | Gera o nome do arquivo para um personagem
getContextFileName :: T.Text -> FilePath
getContextFileName charName = T.unpack charName ++ C.configContextFileExtension C.defaultConfig

-- | Cria um novo contexto
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
                    let defaultResources = Resources
                            { spirit = C.configDefaultResourceSpirit cfg
                            , health = C.configDefaultResourceHealth cfg
                            , supply = C.configDefaultResourceSupply cfg
                            , momentum = C.configDefaultResourceMomentum cfg
                            , experience = C.configDefaultResourceExperience cfg
                            }

                    let character = MainCharacter
                            { name = charName
                            , attributes = attrs
                            , resources = defaultResources
                            }

                    let context = Context
                            { mainCharacter = character
                            , world = World []
                            , sessionLog = []
                            , progressTracks = []
                            , activeBonuses = []
                            , bonds = []
                            }

                    saveResult <- saveContext context
                    case saveResult of
                        Left err -> return $ Left err
                        Right _ -> do
                            cache <- getContextCache
                            modifyMVar cache $ \_ -> return (Just context, ())
                            return $ Right context

-- | Carrega contexto de arquivo
loadContext :: T.Text -> IO (Either ContextError Context)
loadContext charName = do
    let fileName = getContextFileName charName
    exists <- doesFileExist fileName

    if not exists
        then return $ Left (FileError $ "Arquivo não encontrado: " ++ fileName)
        else do
            result <- SafeIO.safeReadFile fileName
            case result of
                Left err -> return $ Left (FileError $ show err)
                Right contents ->
                    case decode contents of
                        Nothing -> return $ Left (ParseError "Erro ao parsear JSON")
                        Just ctx -> do
                            cache <- getContextCache
                            modifyResult <- safeModifyMVar cache $ \_ ->
                                return (Just ctx, ())
                            case modifyResult of
                                Left err -> return $ Left (FileError $ "Erro ao atualizar contexto: " ++ show err)
                                Right _ -> return $ Right ctx

-- | Salva contexto em arquivo
saveContext :: Context -> IO (Either ContextError FilePath)
saveContext ctx = do
    let charName = name . mainCharacter $ ctx
    let fileName = getContextFileName charName
    let jsonContent = encode ctx

    result <- SafeIO.safeWriteFile fileName jsonContent
    case result of
        Left err -> return $ Left (FileError $ show err)
        Right _ -> return $ Right fileName

-- | Obtém contexto atual
getCurrentContext :: IO (Maybe Context)
getCurrentContext = do
    cache <- getContextCache
    readMVar cache

-- | Atualiza atributos do personagem
updateAttributes :: Context -> Attributes -> IO Context
updateAttributes ctx newAttrs = do
    let character = mainCharacter ctx
    let updatedCharacter = character { attributes = newAttrs }
    let updatedCtx = ctx { mainCharacter = updatedCharacter }

    cache <- getContextCache
    modifyMVar cache $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Atualiza recursos do personagem
updateResources :: Context -> Resources -> IO Context
updateResources ctx newRes = do
    let character = mainCharacter ctx
    let updatedCharacter = character { resources = newRes }
    let updatedCtx = ctx { mainCharacter = updatedCharacter }

    cache <- getContextCache
    modifyMVar cache $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona dados ao mundo
addWorldData :: Context -> T.Text -> IO Context
addWorldData ctx newData = do
    let world' = world ctx
    let worldData = wData world'
    let updatedWorld = world' { wData = newData : worldData }
    let updatedCtx = ctx { world = updatedWorld }

    cache <- getContextCache
    modifyMVar cache $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona entrada ao log
addLogEntry :: Context -> T.Text -> IO Context
addLogEntry ctx logEntry = do
    let currentLog = sessionLog ctx
    let updatedCtx = ctx { sessionLog = logEntry : currentLog }

    cache <- getContextCache
    modifyMVar cache $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Obtém todos os logs do contexto
getSessionLog :: Context -> [T.Text]
getSessionLog = sessionLog

-- | Limpa o log do contexto
clearSessionLog :: Context -> IO Context
clearSessionLog ctx = do
    let updatedCtx = ctx { sessionLog = [] }

    cache <- getContextCache
    modifyMVar cache $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona progress track ao contexto
addProgressTrack :: Context -> Progress.ProgressTrack -> IO Context
addProgressTrack _ctx track = do
    cache <- getContextCache
    modifyMVar cache $ \maybeCtx -> do
        case maybeCtx of
            Nothing -> return (Just _ctx, _ctx)
            Just ctx -> do
                let tracks = progressTracks ctx
                let updatedCtx = ctx { progressTracks = track : tracks }
                return (Just updatedCtx, updatedCtx)

-- | Atualiza progress track existente
updateProgressTrack :: Context -> T.Text -> Progress.ProgressTrack -> IO Context
updateProgressTrack _ctx trackName updatedTrack = do
    cache <- getContextCache
    modifyMVar cache $ \maybeCtx -> do
        case maybeCtx of
            Nothing -> return (Just _ctx, _ctx)
            Just ctx -> do
                let tracks = progressTracks ctx
                let newTracks = map (\t -> if Progress.trackName t == trackName then updatedTrack else t) tracks
                let updatedCtx = ctx { progressTracks = newTracks }
                return (Just updatedCtx, updatedCtx)

-- | Obtém progress track por nome
getProgressTrack :: Context -> T.Text -> Maybe Progress.ProgressTrack
getProgressTrack ctx trackName =
    let tracks = progressTracks ctx
    in findTrack tracks
  where
    findTrack [] = Nothing
    findTrack (t:ts)
      | Progress.trackName t == trackName = Just t
      | otherwise = findTrack ts

-- | Remove progress track
removeProgressTrack :: Context -> T.Text -> IO Context
removeProgressTrack ctx trackName = do
    let tracks = progressTracks ctx
    let newTracks = filter (\t -> Progress.trackName t /= trackName) tracks
    let updatedCtx = ctx { progressTracks = newTracks }

    cache <- getContextCache
    modifyMVar cache $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Adiciona bônus ativo
addBonus :: Context -> ActiveBonus -> IO Context
addBonus ctx bonus = do
    let bonuses = activeBonuses ctx
    let updatedCtx = ctx { activeBonuses = bonus : bonuses }

    cache <- getContextCache
    modifyMVar cache $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Remove bônus específico
removeBonus :: Context -> ActiveBonus -> IO Context
removeBonus ctx bonus = do
    let bonuses = activeBonuses ctx
    let newBonuses = filter (/= bonus) bonuses
    let updatedCtx = ctx { activeBonuses = newBonuses }

    cache <- getContextCache
    modifyMVar cache $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Obtém bônus aplicáveis a um move
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

-- | Consome bônus de uso único
consumeBonuses :: Context -> Maybe T.Text -> IO Context
consumeBonuses ctx maybeMoveName = do
    let bonuses = activeBonuses ctx
    let (toConsume, toKeep) = partition (shouldConsume maybeMoveName) bonuses

    mapM_ (\b -> putStrLn $ "  [Bônus consumido: " ++ T.unpack (bonusDescription b) ++ " +" ++ show (bonusValue b) ++ "]") toConsume

    let updatedCtx = ctx { activeBonuses = toKeep }
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

-- | Limpa todos os bônus
clearBonuses :: Context -> IO Context
clearBonuses ctx = do
    let updatedCtx = ctx { activeBonuses = [] }
    cache <- getContextCache
    modifyMVar cache $ \_ -> return (Just updatedCtx, ())
    return updatedCtx

-- | Verifica se tem bond com nome
hasBond :: Context -> T.Text -> Bool
hasBond ctx bn =
    any (\b -> bondName b == bn) (bonds ctx)

-- | Lista todos os bonds
listBonds :: Context -> IO (Either ContextError BondProcessingResponse)
listBonds ctx = return successResponse where
    successResponse = Right $ Response { bondProcessingResult = ctx, systemMessage = T.unlines (ctx&bonds&map bondName) }

-- | Remove arquivo de contexto
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

-- | Adiciona bond
addBond :: Context -> Bond -> IO (Either ContextError BondProcessingResponse)
addBond _ctx b = do
    cache <- getContextCache
    modifyMVar cache $ \maybeCtx -> do
        case maybeCtx of
            Nothing -> return (Just _ctx, errorResponse)
            Just ctx -> do
                let bonds' = bonds ctx
                let updatedCtx = ctx { bonds = b : bonds' }
                return (Just updatedCtx, successResponse updatedCtx)
  where
    successResponse ctx = Right $ Response { bondProcessingResult = ctx, systemMessage = "Bond added successfully" }
    errorResponse = Left $ FileError "Context not found"

-- | Atualiza bond existente
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
                                                in b { bondNotes = appendedNotes }
                                            else b
                                    )
                                    bonds'
                        let noteMessage = "~{ " <> bondNameToUpdate <> " - " <> newNote <> " }~"
                        let updatedCtx =
                                ctx
                                    { bonds = newBonds
                                    , sessionLog = noteMessage : sessionLog ctx
                                    }
                        saveResult <- saveContext updatedCtx
                        case saveResult of
                            Left err -> return (Just ctx, Left err)
                            Right _ -> return (Just updatedCtx, successResponse updatedCtx noteMessage)
  where
    successResponse ctx noteMessage = Right $ Response { bondProcessingResult = ctx, systemMessage = noteMessage }
    errorResponse = Left $ FileError "Context not found"
    bondNotFoundError = Left $ FileError "Bond not found"

    appendNote :: T.Text -> T.Text -> T.Text
    appendNote existing newNote
        | T.null newNote = existing
        | T.null existing = newNote
        | otherwise = existing <> "\n---\n" <> newNote

-- | Remove bond
removeBond :: Context -> T.Text -> IO (Either ContextError BondProcessingResponse)
removeBond _ctx bn = do
    cache <- getContextCache
    modifyMVar cache $ \maybeCtx -> do
        case maybeCtx of
            Nothing -> return (Just _ctx, errorResponse)
            Just ctx -> do
                let bonds' = bonds ctx
                let newBonds = filter (\b -> bondName b /= bn) bonds'
                let updatedCtx = ctx { bonds = newBonds }
                return (Just updatedCtx, successResponse updatedCtx)
  where
    successResponse ctx = Right $ Response { bondProcessingResult = ctx, systemMessage = "Bond removed successfully" }
    errorResponse = Left $ FileError "Context not found"

-- | Lista notas de um bond
listBondNotes :: Context -> T.Text -> IO (Either ContextError BondProcessingResponse)
listBondNotes _ctx bondNameParam = return successResponse where
    successResponse = Right $ Response { bondProcessingResult = _ctx, systemMessage = 
        T.unlines (_ctx&bonds&filter (\b -> bondName b == bondNameParam)&map bondNotes) }

-- | Processa comando de bond
processBondCommand :: BondCommand -> IO (Either ContextError BondProcessingResponse)
processBondCommand BondCommand { bondCommandType = cmdType, bond = param } = do
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
        RemoveBond -> removeBond ctx (param&bondName)
        UpdateBondNotes bondNameParam -> updateBond ctx bondNameParam param
        ListBondNotes bondNameParam -> listBondNotes ctx bondNameParam