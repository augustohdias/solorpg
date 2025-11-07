{-# LANGUAGE OverloadedStrings #-}
{- | MoveContract - Especificação do serviço de Moves do Ironsworn
     
     Este serviço gerencia a execução de Moves (movimentos) do Ironsworn,
     incluindo rolagens automáticas, avaliação de resultados e execução
     de consequências.
     
     Este módulo deve ser importado qualificado:
     > import qualified System.MoveContract as Move
-}
module System.MoveContract
  ( -- * Abstract handle
    Handle (..)
    
    -- * Pure types
  , Stat (..)
  , MoveResult (..)
  
    -- * Derived functions
  , statToText
  ) where

import qualified Data.Text as T
import qualified System.GameContextContract as GameContext
import qualified System.DiceContract as Dice
import System.ConsequenceContract (Consequence(..), Choice(..), MoveType(..))

-- | Atributos que podem ser usados em Moves
data Stat
  = Iron
  | Edge
  | Heart
  | Shadow
  | Wits
  deriving (Eq, Show, Read)

-- | Resultado de execução de um Move
data MoveResult = MoveResult
  { moveExecuted :: MoveType
  , rollResult :: Dice.RollResult
  , actionDie :: Int
  , challengeDice :: (Int, Int)
  , modifier :: Int
  , matchOccurred :: Bool
  , consequencesApplied :: [Consequence]
  } deriving (Show)

-- Consequence e Choice agora vêm de ConsequenceContract

-- | Handle abstrato para o serviço de Moves
data Handle = Handle
  { -- | Executa um move com stat opcional - retorna apenas as consequências
    executeMove :: !(MoveType -> Maybe Stat -> GameContext.Attributes -> GameContext.Resources -> IO [Consequence])
    
    -- | Executa um move a partir de rolagem já feita - retorna apenas as consequências
  , executeMoveWithRoll :: !(MoveType -> Maybe Stat -> Int -> (Int, Int) -> GameContext.Attributes -> GameContext.Resources -> IO [Consequence])
  
    -- | Mostra menu de escolhas e obtém resposta
  , showChoices :: !([Choice] -> IO (Maybe Choice))
  
    -- | Parse nome de move a partir de texto
  , parseMoveType :: !(T.Text -> Maybe MoveType)
  
    -- | Parse stat a partir de texto
  , parseStat :: !(T.Text -> Maybe Stat)
  }

-- | Converte Stat para texto
statToText :: Stat -> T.Text
statToText Iron = "iron"
statToText Edge = "edge"
statToText Heart = "heart"
statToText Shadow = "shadow"
statToText Wits = "wits"
