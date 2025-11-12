{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module System.Progress
  ( ProgressTrack (..)
  , ChallengeRank (..)
  , ProgressType (..)
  , ProgressRollResult (..)
  , ProgressCompletionResult (..)
  , ProgressRollExecutionResult (..)
  , newProgressTrack
  , getProgressScore
  , getTicksForRank
  , progressPercentage
  , markProgress
  , rollProgress
  , completeProgressRoll
  , markProgressTicks
  , completeTrack
  , clearTrack
  , executeProgressRoll
  
  , evaluateProgressRoll
  , getRankExperienceValue
  ) where

import qualified System.Dice as Dice
import qualified System.Constants as C
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)


data ChallengeRank
  = Troublesome  
  | Dangerous    
  | Formidable   
  | Extreme      
  | Epic         
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)


data ProgressType
  = Vow          
  | Combat       
  | Journey      
  | Bond         
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)



data ProgressTrack = ProgressTrack
  { trackName :: !T.Text            
  , trackType :: !ProgressType      
  , trackRank :: !ChallengeRank     
  , trackTicks :: !Int              
  , trackCompleted :: !Bool         
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ProgressRollResult = ProgressRollResult
  { progressScore :: Int                    
  , progressChallengeDice :: (Int, Int)     
  , progressRollResult :: Dice.RollResult  
  , progressMatch :: Bool                   
  } deriving (Eq, Show)


data ProgressCompletionResult = ProgressCompletionResult
  { completionTrack :: ProgressTrack
  , experienceGained :: Int
  , completionMessage :: T.Text
  } deriving (Show)


data ProgressRollExecutionResult = ProgressRollExecutionResult
  { executionNarrativeMessage :: T.Text      
  , executionUpdatedTrack :: ProgressTrack   
  , executionExperienceGained :: Int         
  , executionSystemMessage :: Maybe T.Text  
  } deriving (Show)


newProgressTrack :: T.Text -> ProgressType -> ChallengeRank -> ProgressTrack
newProgressTrack name pType rank = ProgressTrack
  { trackName = name
  , trackType = pType
  , trackRank = rank
  , trackTicks = 0
  , trackCompleted = False
  }


getProgressScore :: ProgressTrack -> Int
getProgressScore track = trackTicks track `div` 4


getTicksForRank :: ChallengeRank -> Int
getTicksForRank Troublesome = 12  
getTicksForRank Dangerous = 8     
getTicksForRank Formidable = 4    
getTicksForRank Extreme = 2       
getTicksForRank Epic = 1          


progressPercentage :: ProgressTrack -> Double
progressPercentage track = 
  (fromIntegral (trackTicks track) / 40.0) * 100.0


markProgress :: ProgressTrack -> IO ProgressTrack
markProgress track
  | trackCompleted track = do
      putStrLn "  → Track já está completo!"
      return track
  | otherwise = do
      let ticksToAdd = getTicksForRank (trackRank track)
      let newTicks = min 40 (trackTicks track + ticksToAdd)
      let boxesFilled = newTicks `div` 4
      
      putStrLn $ "  → Marcou " ++ show ticksToAdd ++ " ticks"
      putStrLn $ "  → Progresso: " ++ show boxesFilled ++ "/10 boxes (" ++ show newTicks ++ "/40 ticks)"
      
      return $ track { trackTicks = newTicks }


markProgressTicks :: ProgressTrack -> Int -> IO ProgressTrack
markProgressTicks track ticksToAdd
  | trackCompleted track = return track
  | otherwise = do
      let newTicks = min 40 (trackTicks track + ticksToAdd)
      return $ track { trackTicks = newTicks }


rollProgress :: ProgressTrack -> IO ProgressRollResult
rollProgress track = do
  let score = getProgressScore track
  
  
  rolls <- Dice.roll (T.pack "2d10")
  
  case rolls of
    [(_, ch1), (_, ch2)] -> do
      let rollResult = evaluateProgressRoll score ch1 ch2
      let isMatch = ch1 == ch2     
      return $ ProgressRollResult
        { progressScore = score
        , progressChallengeDice = (ch1, ch2)
        , progressRollResult = rollResult
        , progressMatch = isMatch
        }
    _ -> do
      return $ ProgressRollResult
        { progressScore = score
        , progressChallengeDice = (0, 0)
        , progressRollResult = Dice.InvalidRoll
        , progressMatch = False
        }


completeTrack :: ProgressTrack -> IO ProgressTrack
completeTrack track = return $ track { trackCompleted = True }


completeProgressRoll :: ProgressTrack -> ProgressRollResult -> IO ProgressCompletionResult
completeProgressRoll track rollResult = do
  case progressRollResult rollResult of
    Dice.StrongHit -> do
      let completedTrack = track { trackCompleted = True }
      let rankValue = getRankExperienceValue (trackRank track)
      let msg = "[+] Experiência adicionada: +" <> T.pack (show rankValue)
      return $ ProgressCompletionResult completedTrack rankValue msg
      
    Dice.WeakHit -> do
      let completedTrack = track { trackCompleted = True }
      let baseValue = getRankExperienceValue (trackRank track)
      let expValue = max 1 (baseValue - 1)
      let msg = "[+] Experiência adicionada: +" <> T.pack (show expValue)
      return $ ProgressCompletionResult completedTrack expValue msg
      
    Dice.Miss -> do
      if trackType track == Vow
        then do
          let clearedTrack = track { trackTicks = 0 }
          let msg = "Progresso limpo. Escolha: reafirmar voto (-2 spirit) ou abandonar."
          return $ ProgressCompletionResult clearedTrack 0 msg
        else do
          let msg = "Progress roll falhou!"
          return $ ProgressCompletionResult track 0 msg
          
    Dice.InvalidRoll -> do
      let msg = "Erro na rolagem"
      return $ ProgressCompletionResult track 0 msg


getRankExperienceValue :: ChallengeRank -> Int
getRankExperienceValue Troublesome = 1
getRankExperienceValue Dangerous = 2
getRankExperienceValue Formidable = 3
getRankExperienceValue Extreme = 4
getRankExperienceValue Epic = 5


clearTrack :: ProgressTrack -> IO ProgressTrack
clearTrack track = do
  putStrLn $ "  ○ Track limpo: " ++ T.unpack (trackName track)
  return $ track { trackTicks = 0, trackCompleted = False }


evaluateProgressRoll :: Int -> Int -> Int -> Dice.RollResult
evaluateProgressRoll score ch1 ch2
  | score > ch1 && score > ch2 = Dice.StrongHit
  | score > ch1 || score > ch2 = Dice.WeakHit
  | otherwise = Dice.Miss


formatProgressRollResult :: ProgressRollResult -> ProgressType -> T.Text
formatProgressRollResult result pType =
  let score = progressScore result
      (ch1, ch2) = progressChallengeDice result
      rollResult = progressRollResult result
      hasMatch = progressMatch result

      resultMsg = case rollResult of
        Dice.StrongHit -> "[+] Sucesso Total!"
        Dice.WeakHit -> "[~] Sucesso Parcial!"
        Dice.Miss -> "[X] Falha!"
        Dice.InvalidRoll -> "INVALID"

      matchMsg = if hasMatch then "\n[!]" else ""

      interpretation = case (pType, rollResult) of
        (Vow, Dice.StrongHit) -> C.vowStrongHit C.progressInterpretation
        (Vow, Dice.WeakHit) -> C.vowWeakHit C.progressInterpretation
        (Vow, Dice.Miss) -> C.vowMiss C.progressInterpretation
        (Combat, Dice.StrongHit) -> C.combatStrongHit C.progressInterpretation
        (Combat, Dice.WeakHit) -> C.combatWeakHit C.progressInterpretation
        (Combat, Dice.Miss) -> C.combatMiss C.progressInterpretation
        (Journey, Dice.StrongHit) -> C.journeyStrongHit C.progressInterpretation
        (Journey, Dice.WeakHit) -> C.journeyWeakHit C.progressInterpretation
        (Journey, Dice.Miss) -> C.journeyMiss C.progressInterpretation
        _ -> ""

      header =
        "\n=== Progress Roll ===\n"
          <> "Progress Score: "
          <> T.pack (show score)
          <> "\n"
          <> "Challenge Dice: "
          <> T.pack (show ch1)
          <> ", "
          <> T.pack (show ch2)
          <> "\n"
          <> "Resultado: "
          <> T.pack resultMsg
          <> matchMsg
          <> "\n"
   in header <> T.pack interpretation


executeProgressRoll :: ProgressTrack -> IO ProgressRollExecutionResult
executeProgressRoll track = do
  rollResult <- rollProgress track
  let formattedNarrative = formatProgressRollResult rollResult (trackType track)
  completionResult <- completeProgressRoll track rollResult
  
  let updatedTrack = completionTrack completionResult
  let expGained = experienceGained completionResult
  let completionMsg = completionMessage completionResult
  
  let systemMsg = if T.null completionMsg
        then Nothing
        else Just completionMsg
  
  return $ ProgressRollExecutionResult
    { executionNarrativeMessage = formattedNarrative
    , executionUpdatedTrack = updatedTrack
    , executionExperienceGained = expGained
    , executionSystemMessage = systemMsg
    }

