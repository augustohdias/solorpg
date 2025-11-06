{-# LANGUAGE InstanceSigs #-}
module System.DiceContract 
    ( Handle (..)
    , DiceType (..)
    , Read (readsPrec)
    , RollResult (..)
    , roll
    ) where
import qualified Data.Text as T

data DiceType = D2 | D4 | D6 | D8 | D10 | D12 | D20 | D100
  deriving (Eq, Show)

instance Read DiceType where
  readsPrec :: Int -> ReadS DiceType
  readsPrec _ "2" = [(D2, "")]
  readsPrec _ "4" = [(D4, "")]
  readsPrec _ "6" = [(D6, "")]
  readsPrec _ "8" = [(D8, "")]
  readsPrec _ "10" = [(D10, "")]
  readsPrec _ "12" = [(D12, "")]
  readsPrec _ "20" = [(D20, "")]
  readsPrec _ "100" = [(D100, "")]
  readsPrec _ _ = []

data Handle = Handle 
    { rollWith :: (Int, DiceType) -> IO [(DiceType, Int)]
    , parseDiceString :: T.Text -> Maybe [(Int, DiceType)]
    }

roll :: Handle -> T.Text -> IO [(DiceType, Int)]
roll diceHandler s = do
  case parseDiceString diceHandler s of
    Just diceList -> fmap concat (mapM (rollWith diceHandler) diceList)
    Nothing -> return []

-- | Resultado de um Action Roll (Ironsworn)
data RollResult
  = StrongHit    -- ^ Action > ambos challenge dice
  | WeakHit      -- ^ Action > apenas 1 challenge die
  | Miss         -- ^ Action <= ambos challenge dice
  | InvalidRoll  -- ^ Rolagem inválida
  deriving (Eq, Show)