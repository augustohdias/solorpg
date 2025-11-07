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
import qualified System.Util.Parser as Parser
import qualified Data.Text as T
import System.DiceContract (RollResult (..))

-- | Cria novo handle para o serviço de Progress
newHandle :: Dice.Handle -> IO Progress.Handle
newHandle diceH = do
  return $ Progress.Handle
    { Progress.markProgress = markProgressImpl diceH
    , Progress.rollProgress = rollProgressImpl diceH
    , Progress.completeProgressRoll = completeProgressRollImpl
    , Progress.markProgressTicks = markProgressTicksImpl
    , Progress.completeTrack = completeTrackImpl
    , Progress.clearTrack = clearTrackImpl
    , Progress.executeProgressRoll = executeProgressRollImpl diceH
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
      return track
  | otherwise = do
      let newTicks = min 40 (Progress.trackTicks track + ticksToAdd)
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
      return $ Progress.ProgressRollResult
        { Progress.progressScore = score
        , Progress.progressChallengeDice = (ch1, ch2)
        , Progress.progressRollResult = rollResult
        , Progress.progressMatch = isMatch
        }
    _ -> do
      return $ Progress.ProgressRollResult
        { Progress.progressScore = score
        , Progress.progressChallengeDice = (0, 0)
        , Progress.progressRollResult = InvalidRoll
        , Progress.progressMatch = False
        }

-- | Completa um track
completeTrackImpl :: Progress.ProgressTrack -> IO Progress.ProgressTrack
completeTrackImpl track = do
  return $ track { Progress.trackCompleted = True }

-- | Completa track e calcula experiência baseado no resultado
completeProgressRollImpl :: Progress.ProgressTrack -> Progress.ProgressRollResult -> IO Progress.ProgressCompletionResult
completeProgressRollImpl track rollResult = do
  case Progress.progressRollResult rollResult of
    StrongHit -> do
      let completedTrack = track { Progress.trackCompleted = True }
      let rankValue = getRankExperienceValue (Progress.trackRank track)
      let msg = "[+] Experiência adicionada: +" <> T.pack (show rankValue)
      return $ Progress.ProgressCompletionResult completedTrack rankValue msg
      
    WeakHit -> do
      let completedTrack = track { Progress.trackCompleted = True }
      let baseValue = getRankExperienceValue (Progress.trackRank track)
      let expValue = max 1 (baseValue - 1)
      let msg = "[+] Experiência adicionada: +" <> T.pack (show expValue)
      return $ Progress.ProgressCompletionResult completedTrack expValue msg
      
    Miss -> do
      if Progress.trackType track == Progress.Vow
        then do
          let clearedTrack = track { Progress.trackTicks = 0 }
          let msg = "Progresso limpo. Escolha: reafirmar voto (-2 spirit) ou abandonar."
          return $ Progress.ProgressCompletionResult clearedTrack 0 msg
        else do
          let msg = "Progress roll falhou!"
          return $ Progress.ProgressCompletionResult track 0 msg
          
    InvalidRoll -> do
      let msg = "Erro na rolagem"
      return $ Progress.ProgressCompletionResult track 0 msg

-- | Obtém valor de experiência baseado no rank
getRankExperienceValue :: Progress.ChallengeRank -> Int
getRankExperienceValue Progress.Troublesome = 1
getRankExperienceValue Progress.Dangerous = 2
getRankExperienceValue Progress.Formidable = 3
getRankExperienceValue Progress.Extreme = 4
getRankExperienceValue Progress.Epic = 5

-- | Limpa um track (reseta progresso)
clearTrackImpl :: Progress.ProgressTrack -> IO Progress.ProgressTrack
clearTrackImpl track = do
  putStrLn $ "  ○ Track limpo: " ++ T.unpack (Progress.trackName track)
  return $ track { Progress.trackTicks = 0, Progress.trackCompleted = False }

-- | Avalia progress roll
evaluateProgressRoll :: Int -> Int -> Int -> Dice.RollResult
evaluateProgressRoll score ch1 ch2
  | score > ch1 && score > ch2 = StrongHit
  | score > ch1 || score > ch2 = WeakHit
  | otherwise = Miss

-- | Executa progress roll completo e retorna resultado formatado para ActionService.
-- Realiza roll, formata mensagem narrativa, processa conclusão e retorna todas
-- as informações necessárias para o ActionService decidir o que enviar para TUI.
executeProgressRollImpl :: Dice.Handle -> Progress.ProgressTrack -> IO Progress.ProgressRollExecutionResult
executeProgressRollImpl diceH track = do
  rollResult <- rollProgressImpl diceH track
  let formattedNarrative = Parser.formatProgressRollResult rollResult (Progress.trackType track)
  completionResult <- completeProgressRollImpl track rollResult
  
  let updatedTrack = Progress.completionTrack completionResult
  let expGained = Progress.experienceGained completionResult
  let completionMsg = Progress.completionMessage completionResult
  
  let systemMsg = if T.null completionMsg
        then Nothing
        else Just completionMsg
  
  return $ Progress.ProgressRollExecutionResult
    { Progress.executionNarrativeMessage = formattedNarrative
    , Progress.executionUpdatedTrack = updatedTrack
    , Progress.executionExperienceGained = expGained
    , Progress.executionSystemMessage = systemMsg
    }