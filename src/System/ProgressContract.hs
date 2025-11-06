{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{- | ProgressContract - Sistema de Progress Tracks do Ironsworn
     
     Progress Tracks são usados para:
     - Vows (juramentos/missões)
     - Combat (combate)
     - Journeys (jornadas)
     
     Baseado nas regras oficiais do Ironsworn Rulebook.
     
     Este módulo deve ser importado qualificado:
     > import qualified System.ProgressContract as Progress
-}
module System.ProgressContract
  ( -- * Abstract handle
    Handle (..)
    
    -- * Pure types
  , ProgressTrack (..)
  , ChallengeRank (..)
  , ProgressType (..)
  , ProgressRollResult (..)
  
    -- * Derived functions
  , newProgressTrack
  , getProgressScore
  , getTicksForRank
  , progressPercentage
  ) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified System.DiceContract as Dice

-- | Rank de desafio (dificuldade)
data ChallengeRank
  = Troublesome  -- ^ Simples - 3 progress (12 ticks) por mark
  | Dangerous    -- ^ Típico - 2 progress (8 ticks) por mark
  | Formidable   -- ^ Difícil - 1 progress (4 ticks) por mark
  | Extreme      -- ^ Muito difícil - 2 ticks por mark
  | Epic         -- ^ Lendário - 1 tick por mark
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Tipo de progress track
data ProgressType
  = Vow          -- ^ Juramento/missão
  | Combat       -- ^ Combate
  | Journey      -- ^ Jornada
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

-- | Progress Track
-- 10 boxes, cada box tem 4 ticks (total: 40 ticks)
data ProgressTrack = ProgressTrack
  { trackName :: !T.Text            -- ^ Nome do track (ex: "Vingar meu pai")
  , trackType :: !ProgressType      -- ^ Tipo do track
  , trackRank :: !ChallengeRank     -- ^ Rank de dificuldade
  , trackTicks :: !Int              -- ^ Ticks marcados (0-40)
  , trackCompleted :: !Bool         -- ^ Se foi completado
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Resultado de um progress roll
data ProgressRollResult = ProgressRollResult
  { progressScore :: Int                    -- ^ Score (boxes completos)
  , progressChallengeDice :: (Int, Int)     -- ^ Challenge dice rolados
  , progressRollResult :: Dice.RollResult  -- ^ Resultado (Strong/Weak/Miss)
  , progressMatch :: Bool                   -- ^ Se houve match
  } deriving (Eq, Show)

-- | Handle abstrato para o serviço de Progress
data Handle = Handle
  { -- | Marca progresso em um track
    markProgress :: !(ProgressTrack -> IO ProgressTrack)
    
    -- | Faz progress roll (rola 2d10 vs progress score)
  , rollProgress :: !(ProgressTrack -> IO ProgressRollResult)
  
    -- | Marca progresso customizado (quantidade específica de ticks)
  , markProgressTicks :: !(ProgressTrack -> Int -> IO ProgressTrack)
  
    -- | Completa um track (quando progress roll é sucesso)
  , completeTrack :: !(ProgressTrack -> IO ProgressTrack)
  
    -- | Limpa um track (remove progresso)
  , clearTrack :: !(ProgressTrack -> IO ProgressTrack)
  }

-- | Cria novo progress track
newProgressTrack :: T.Text -> ProgressType -> ChallengeRank -> ProgressTrack
newProgressTrack name pType rank = ProgressTrack
  { trackName = name
  , trackType = pType
  , trackRank = rank
  , trackTicks = 0
  , trackCompleted = False
  }

-- | Calcula progress score (número de boxes completos)
-- Cada box tem 4 ticks
getProgressScore :: ProgressTrack -> Int
getProgressScore track = trackTicks track `div` 4

-- | Retorna quantos ticks marcar por rank
getTicksForRank :: ChallengeRank -> Int
getTicksForRank Troublesome = 12  -- 3 boxes (3 * 4 ticks)
getTicksForRank Dangerous = 8     -- 2 boxes (2 * 4 ticks)
getTicksForRank Formidable = 4    -- 1 box (1 * 4 ticks)
getTicksForRank Extreme = 2       -- 2 ticks
getTicksForRank Epic = 1          -- 1 tick

-- | Calcula porcentagem de progresso
progressPercentage :: ProgressTrack -> Double
progressPercentage track = 
  (fromIntegral (trackTicks track) / 40.0) * 100.0

