{-# LANGUAGE InstanceSigs #-}
module System.DiceContract 
    ( Handle (..)
    , DiceType (..)
    , Read (readsPrec)
    , RollResult (..)
    , ChallengeResult (..)
    , roll
    , formatChallengeResult
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

data ChallengeResult = ChallengeResult
  { challengeActionDie :: Int
  , challengeDie1 :: Int
  , challengeDie2 :: Int
  , challengeRollResult :: RollResult
  , challengeMatch :: Bool
  } deriving (Show)

data Handle = Handle 
    { rollWith :: (Int, DiceType) -> IO [(DiceType, Int)]
    , parseDiceString :: T.Text -> Maybe [(Int, DiceType)]
    , challengeRoll :: IO (Either T.Text ChallengeResult)
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