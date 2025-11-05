{-# LANGUAGE OverloadedStrings #-}

module System.Impl.DiceService (newHandle) where

import qualified System.DiceContract as Dice
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Control.Monad (replicateM)
import System.Random (randomRIO)

newHandle :: IO Dice.Handle
newHandle = return $ Dice.Handle 
    { Dice.parseDiceString = \input -> 
        let parts = T.splitOn (T.pack ",") input
        in mapM parseSingleDice parts
    , Dice.rollWith = \(n, diceType) -> do
        results <- replicateM n (rollDice diceType)
        return [(diceType, r) | r <- results]
    }
    where rollDice Dice.D2   = randomRIO (1, 2)
          rollDice Dice.D4   = randomRIO (1, 4)
          rollDice Dice.D6   = randomRIO (1, 6)
          rollDice Dice.D8   = randomRIO (1, 8)
          rollDice Dice.D10  = randomRIO (1, 10)
          rollDice Dice.D12  = randomRIO (1, 12)
          rollDice Dice.D20  = randomRIO (1, 20)
          rollDice Dice.D100 = randomRIO (1, 100)

parseSingleDice :: T.Text -> Maybe (Int, Dice.DiceType)
parseSingleDice txt = 
    case T.splitOn "d" txt of
        [numText, diceText] -> do
            num <- case decimal numText of
                Right (n, _) -> Just n
                Left _ -> Nothing
            diceType <- parseDiceType diceText
            return (num, diceType)
        _ -> Nothing

parseDiceType :: T.Text -> Maybe Dice.DiceType
parseDiceType txt = case reads (T.unpack txt) of
    [(diceType, "")] -> Just diceType
    _ -> Nothing