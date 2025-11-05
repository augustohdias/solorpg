{-# LANGUAGE OverloadedStrings #-}
{- | Implementação do serviço de Progress Tracks
     
     Implementa o sistema de progress tracks do Ironsworn:
     - Marcar progresso baseado em rank
     - Progress rolls (2d10 vs progress score)
     - Gerenciamento de tracks
-}
module System.Impl.ProgressService (newHandle) where

import qualified System.ProgressContract as Progress
import qualified System.DiceContract as Dice
import qualified Data.Text as T
import System.ActionContract (ActionRollResult (..))
import Control.Monad (when)

-- | Cria novo handle para o serviço de Progress
newHandle :: Dice.Handle -> IO Progress.Handle
newHandle diceH = do
  return $ Progress.Handle
    { Progress.markProgress = markProgressImpl diceH
    , Progress.rollProgress = rollProgressImpl diceH
    , Progress.markProgressTicks = markProgressTicksImpl
    , Progress.completeTrack = completeTrackImpl
    , Progress.clearTrack = clearTrackImpl
    }

-- | Marca progresso baseado no rank do track
markProgressImpl :: Dice.Handle -> Progress.ProgressTrack -> IO Progress.ProgressTrack
markProgressImpl _diceH track
  | Progress.trackCompleted track = do
      putStrLn "  → Track já está completo!"
      return track
  | otherwise = do
      let ticksToAdd = Progress.getTicksForRank (Progress.trackRank track)
      let newTicks = min 40 (Progress.trackTicks track + ticksToAdd)
      let boxesFilled = newTicks `div` 4
      
      putStrLn $ "  → Marcou " ++ show ticksToAdd ++ " ticks"
      putStrLn $ "  → Progresso: " ++ show boxesFilled ++ "/10 boxes (" ++ show newTicks ++ "/40 ticks)"
      
      return $ track { Progress.trackTicks = newTicks }

-- | Marca quantidade específica de ticks
markProgressTicksImpl :: Progress.ProgressTrack -> Int -> IO Progress.ProgressTrack
markProgressTicksImpl track ticksToAdd
  | Progress.trackCompleted track = do
      putStrLn "  → Track já está completo!"
      return track
  | otherwise = do
      let newTicks = min 40 (Progress.trackTicks track + ticksToAdd)
      let boxesFilled = newTicks `div` 4
      
      putStrLn $ "  → Marcou " ++ show ticksToAdd ++ " ticks"
      putStrLn $ "  → Progresso: " ++ show boxesFilled ++ "/10 boxes"
      
      return $ track { Progress.trackTicks = newTicks }

-- | Faz progress roll (2d10 vs progress score)
rollProgressImpl :: Dice.Handle -> Progress.ProgressTrack -> IO Progress.ProgressRollResult
rollProgressImpl diceH track = do
  let score = Progress.getProgressScore track
  
  -- Rola apenas 2d10 (sem action die!)
  rolls <- Dice.roll diceH "2d10"
  
  case rolls of
    [(_, ch1), (_, ch2)] -> do
      let rollResult = evaluateProgressRoll score ch1 ch2
      let isMatch = ch1 == ch2
      
      putStrLn "\n=== Progress Roll ==="
      putStrLn $ "Progress Score: " ++ show score ++ " (" ++ show (Progress.trackTicks track) ++ " ticks)"
      putStrLn $ "Challenge Dice: " ++ show ch1 ++ ", " ++ show ch2
      
      when isMatch $
        putStrLn "⚠ MATCH!"
      
      putStrLn $ "\nResultado: " ++ showProgressResult rollResult
      
      return $ Progress.ProgressRollResult
        { Progress.progressScore = score
        , Progress.progressChallengeDice = (ch1, ch2)
        , Progress.progressRollResult = rollResult
        , Progress.progressMatch = isMatch
        }
    _ -> do
      putStrLn "Erro ao rolar dados para progress roll"
      return $ Progress.ProgressRollResult
        { Progress.progressScore = score
        , Progress.progressChallengeDice = (0, 0)
        , Progress.progressRollResult = InvalidRoll
        , Progress.progressMatch = False
        }

-- | Completa um track
completeTrackImpl :: Progress.ProgressTrack -> IO Progress.ProgressTrack
completeTrackImpl track = do
  putStrLn $ "  ✓ Track completado: " ++ T.unpack (Progress.trackName track)
  return $ track { Progress.trackCompleted = True }

-- | Limpa um track (reseta progresso)
clearTrackImpl :: Progress.ProgressTrack -> IO Progress.ProgressTrack
clearTrackImpl track = do
  putStrLn $ "  ○ Track limpo: " ++ T.unpack (Progress.trackName track)
  return $ track { Progress.trackTicks = 0, Progress.trackCompleted = False }

-- | Avalia progress roll
evaluateProgressRoll :: Int -> Int -> Int -> ActionRollResult
evaluateProgressRoll score ch1 ch2
  | score > ch1 && score > ch2 = StrongHit
  | score > ch1 || score > ch2 = WeakHit
  | otherwise = Miss

-- | Formata resultado de progress roll
showProgressResult :: ActionRollResult -> String
showProgressResult StrongHit = "STRONG HIT ✓"
showProgressResult WeakHit = "WEAK HIT ~"
showProgressResult Miss = "MISS ✗"
showProgressResult InvalidRoll = "INVALID"

