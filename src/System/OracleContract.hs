{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{- | OracleContract - Sistema de Oráculos customizáveis do Ironsworn
     
     Oráculos são tabelas de consulta aleatória para geração procedural.
     Carregados de arquivos JSON permitindo customização total.
     
     Este módulo deve ser importado qualificado:
     > import qualified System.OracleContract as Oracle
-}
module System.OracleContract
  ( -- * Abstract handle
    Handle (..)
    
    -- * Pure types
  , Oracle (..)
  , OracleEntry (..)
  , OracleResult (..)
  , OracleError (..)
  ) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Entrada de oráculo (linha da tabela)
data OracleEntry = OracleEntry
  { entryRange :: !(Int, Int)        -- ^ Range de valores (ex: 1-10)
  , entryText :: !T.Text             -- ^ Texto do resultado
  , entryConsequence :: !(Maybe T.Text)  -- ^ Consequência executável opcional
  } deriving (Eq, Show, Generic)

instance ToJSON OracleEntry
instance FromJSON OracleEntry

-- | Tabela de oráculo
data Oracle = Oracle
  { oracleName :: !T.Text           -- ^ Nome do oráculo
  , oracleDescription :: !T.Text    -- ^ Descrição
  , oracleEntries :: ![OracleEntry] -- ^ Entradas da tabela
  , oracleDice :: !T.Text           -- ^ Dado usado (ex: "1d100")
  } deriving (Eq, Show, Generic)

instance ToJSON Oracle
instance FromJSON Oracle

-- | Resultado de consulta ao oráculo
data OracleResult = OracleResult
  { resultOracle :: !T.Text          -- ^ Nome do oráculo consultado
  , resultRoll :: !Int               -- ^ Valor rolado
  , resultText :: !T.Text            -- ^ Texto do resultado
  , resultConsequence :: !(Maybe T.Text)  -- ^ Consequência opcional
  } deriving (Eq, Show)

-- | Erros do sistema de oráculos
data OracleError
  = OracleNotFound T.Text       -- ^ Oráculo não existe
  | InvalidRollValue Int        -- ^ Valor fora do range
  | EmptyOracle                 -- ^ Oráculo sem entradas
  | InvalidFormat String        -- ^ Erro no formato JSON
  deriving (Eq, Show)

-- | Handle abstrato para o serviço de Oráculos
data Handle = Handle
  { -- | Carrega oráculo de arquivo JSON
    loadOracle :: !(FilePath -> IO (Either OracleError Oracle))
    
    -- | Consulta oráculo com valor específico
  , queryOracle :: !(T.Text -> Int -> IO (Either OracleError OracleResult))
  
    -- | Consulta oráculo com rolagem automática
  , rollOracle :: !(T.Text -> IO (Either OracleError OracleResult))
  
    -- | Lista todos os oráculos carregados
  , listOracles :: !(IO [T.Text])
  
    -- | Mostra oráculo completo
  , showOracle :: !(T.Text -> IO (Either OracleError Oracle))
  }

