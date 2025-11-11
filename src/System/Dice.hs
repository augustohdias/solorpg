{-# LANGUAGE OverloadedStrings #-}
module System.Dice
    ( DiceType (..)
    , RollResult (..)
    , ChallengeResult (..)
    , roll
    , challengeRoll
    , formatChallengeResult
    ) where

import qualified Data.Text as T
import Data.Text.Read (decimal)
import Control.Monad (replicateM)
import System.Random (randomRIO)

data DiceType = D2 | D4 | D6 | D8 | D10 | D12 | D20 | D100
  deriving (Eq, Show, Read)

-- | Resultado de um Action Roll (Ironsworn)
data RollResult
  = StrongHit    -- ^ Action > ambos challenge dice
  | WeakHit      -- ^ Action > apenas 1 challenge die
  | Miss         -- ^ Action <= ambos challenge dice
  | InvalidRoll  -- ^ Rolagem inválida
  deriving (Eq, Show)

data ChallengeResult = ChallengeResult
  { challengeActionDie :: Int
  , challengeDie1 :: Int
  , challengeDie2 :: Int
  , challengeRollResult :: RollResult
  , challengeMatch :: Bool
  } deriving (Show)

-- | Função auxiliar para rolar um dado
rollDice :: DiceType -> IO Int
rollDice D2   = randomRIO (1, 2)
rollDice D4   = randomRIO (1, 4)
rollDice D6   = randomRIO (1, 6)
rollDice D8   = randomRIO (1, 8)
rollDice D10  = randomRIO (1, 10)
rollDice D12  = randomRIO (1, 12)
rollDice D20  = randomRIO (1, 20)
rollDice D100 = randomRIO (1, 100)

-- | Avalia o resultado de um Action Roll (Ironsworn)
evaluateActionRoll :: Int -> Int -> Int -> RollResult
evaluateActionRoll actionDie challenge1 challenge2
  | actionDie > challenge1 && actionDie > challenge2 = StrongHit
  | actionDie > challenge1 || actionDie > challenge2 = WeakHit
  | otherwise = Miss

-- | Rola dados a partir de uma string (ex: "3d6,2d10")
roll :: T.Text -> IO [(DiceType, Int)]
roll input = do
  case parseDiceString input of
    Just diceList -> fmap concat (mapM rollSingle diceList)
    Nothing -> return []

-- | Rola um único tipo de dado múltiplas vezes
rollSingle :: (Int, DiceType) -> IO [(DiceType, Int)]
rollSingle (n, diceType) = do
  results <- replicateM n (rollDice diceType)
  return [(diceType, r) | r <- results]

-- | Implementa o challenge roll (1d6 + 2d10)
challengeRoll :: IO (Either T.Text ChallengeResult)
challengeRoll = do
  actionDie <- rollDice D6
  ch1 <- rollDice D10
  ch2 <- rollDice D10
  
  let result = evaluateActionRoll actionDie ch1 ch2
  let hasMatch = ch1 == ch2
  
  return $ Right $ ChallengeResult
    { challengeActionDie = actionDie
    , challengeDie1 = ch1
    , challengeDie2 = ch2
    , challengeRollResult = result
    , challengeMatch = hasMatch
    }

-- | Parse string de dados (ex: "3d6,2d10")
parseDiceString :: T.Text -> Maybe [(Int, DiceType)]
parseDiceString input = 
  let parts = T.splitOn (T.pack ",") input
  in mapM parseSingleDice parts

parseSingleDice :: T.Text -> Maybe (Int, DiceType)
parseSingleDice txt = 
    case T.splitOn "d" txt of
        [numText, diceText] -> do
            num <- case decimal numText of
                Right (n, _) -> Just n
                Left _ -> Nothing
            diceType <- parseDiceType diceText
            return (num, diceType)
        _ -> Nothing

parseDiceType :: T.Text -> Maybe DiceType
parseDiceType txt = case T.unpack (T.toLower txt) of
    "2" -> Just D2
    "4" -> Just D4
    "6" -> Just D6
    "8" -> Just D8
    "10" -> Just D10
    "12" -> Just D12
    "20" -> Just D20
    "100" -> Just D100
    _ -> Nothing

-- | Formata o resultado de um challenge roll
formatChallengeResult :: ChallengeResult 
                      -> (Int -> Int -> Int -> String -> String)  -- formatActionRoll
                      -> String  -- challengeStrongHit
                      -> String  -- challengeWeakHit
                      -> String  -- challengeMiss
                      -> String  -- challengeMatch
                      -> T.Text
formatChallengeResult result formatActionRoll strongHit weakHit miss matchMsg =
  let ChallengeResult { challengeActionDie = actionDie
                      , challengeDie1 = ch1
                      , challengeDie2 = ch2
                      , challengeRollResult = rollResult
                      , challengeMatch = hasMatch
                      } = result
  
      resultMsg = case rollResult of
        StrongHit -> "STRONG HIT"
        WeakHit -> "WEAK HIT"
        Miss -> "MISS"
        InvalidRoll -> "INVALID ROLL"
  
      interpretation = case rollResult of
        StrongHit -> strongHit
        WeakHit -> weakHit
        Miss -> miss
        InvalidRoll -> ""
  
      match = if hasMatch then matchMsg else ""
  
  in T.pack $ formatActionRoll actionDie ch1 ch2 resultMsg ++ interpretation ++ match

