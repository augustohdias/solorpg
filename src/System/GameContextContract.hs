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
module System.GameContextContract 
  ( -- * Abstract handle
    Handle (..)
    
    -- * Pure types
  , Context (..)
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
  ) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Maybe (fromMaybe)
import qualified Data.Maybe
import qualified System.ProgressContract as Progress

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

-- | Handle abstrato para o serviço de contexto
data Handle = Handle
    { -- | Cria um novo contexto com personagem principal
      createContext :: !(T.Text -> Attributes -> IO (Either ContextError Context))

      -- | Carrega contexto de um arquivo
    , loadContext :: !(T.Text -> IO (Either ContextError Context))

      -- | Salva o contexto atual em arquivo
    , saveContext :: !(Context -> IO (Either ContextError FilePath))

      -- | Obtém o contexto atual (se existir)
    , getCurrentContext :: !(IO (Maybe Context))

      -- | Atualiza atributos do personagem
    , updateAttributes :: !(Context -> Attributes -> IO Context)

      -- | Atualiza recursos do personagem
    , updateResources :: !(Context -> Resources -> IO Context)

    -- | Adiciona dados ao mundo
    , addWorldData :: !(Context -> T.Text -> IO Context)

    -- | Adiciona evento ao log do contexto
    , addLogEntry :: !(Context -> T.Text -> IO Context)

    -- | Obtém todos os logs do contexto
    , getSessionLog :: !(Context -> [T.Text])

    -- | Limpa o log do contexto (útil para nova sessão)
    , clearSessionLog :: !(Context -> IO Context)
    
    -- | Adiciona progress track ao contexto
    , addProgressTrack :: !(Context -> Progress.ProgressTrack -> IO Context)
    
    -- | Atualiza progress track existente
    , updateProgressTrack :: !(Context -> T.Text -> Progress.ProgressTrack -> IO Context)
    
    -- | Obtém progress track por nome
    , getProgressTrack :: !(Context -> T.Text -> Maybe Progress.ProgressTrack)
    
    -- | Remove progress track
    , removeProgressTrack :: !(Context -> T.Text -> IO Context)
    
    -- | Adiciona bônus ativo
    , addBonus :: !(Context -> ActiveBonus -> IO Context)
    
    -- | Remove bônus específico
    , removeBonus :: !(Context -> ActiveBonus -> IO Context)
    
    -- | Obtém bônus aplicáveis a um move
    , getApplicableBonuses :: !(Context -> Maybe T.Text -> [ActiveBonus])
    
    -- | Consome bônus de uso único (NextRoll, NextMove)
    , consumeBonuses :: !(Context -> Maybe T.Text -> IO Context)
    
    -- | Limpa todos os bônus
    , clearBonuses :: !(Context -> IO Context)
    
    -- | Adiciona bond
    , addBond :: !(Context -> Bond -> IO (Either ContextError BondProcessingResponse))
    
    -- | Remove bond
    , removeBond :: !(Context -> T.Text -> IO (Either ContextError BondProcessingResponse))
    
    -- | Verifica se tem bond com nome
    , hasBond :: !(Context -> T.Text -> Bool)
    
    -- | Lista todos os bonds
    , listBonds :: !(Context -> IO (Either ContextError BondProcessingResponse))
    
    -- | Atualiza bond existente
    , updateBond :: !(Context -> T.Text -> Bond -> IO (Either ContextError BondProcessingResponse))

    -- | Remove o arquivo de contexto
    , deleteContext :: !(T.Text -> IO (Either ContextError ()))

    , processBondCommand :: !(BondCommand -> IO (Either ContextError BondProcessingResponse))
    }

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