{-# LANGUAGE OverloadedStrings #-}
module System.ProgressSpec (spec) where

import Test.Hspec
import qualified System.Progress as Progress
import qualified System.Dice as Dice

spec :: Spec
spec = describe "System.Progress" $ do
  describe "newProgressTrack" $ do
    it "creates a new progress track with correct initial values" $ do
      let track = Progress.newProgressTrack "Test Vow" Progress.Vow Progress.Dangerous
      Progress.trackName track `shouldBe` "Test Vow"
      Progress.trackType track `shouldBe` Progress.Vow
      Progress.trackRank track `shouldBe` Progress.Dangerous
      Progress.trackTicks track `shouldBe` 0
      Progress.trackCompleted track `shouldBe` False

  describe "getTicksForRank" $ do
    it "returns correct ticks for each rank" $ do
      Progress.getTicksForRank Progress.Troublesome `shouldBe` 12
      Progress.getTicksForRank Progress.Dangerous `shouldBe` 8
      Progress.getTicksForRank Progress.Formidable `shouldBe` 4
      Progress.getTicksForRank Progress.Extreme `shouldBe` 2
      Progress.getTicksForRank Progress.Epic `shouldBe` 1

  describe "getProgressScore" $ do
    it "calculates progress score correctly" $ do
      let track1 = Progress.newProgressTrack "Test" Progress.Vow Progress.Dangerous
      Progress.getProgressScore track1 `shouldBe` 0
      
      let track2 = track1 { Progress.trackTicks = 4 }
      Progress.getProgressScore track2 `shouldBe` 1
      
      let track3 = track1 { Progress.trackTicks = 8 }
      Progress.getProgressScore track3 `shouldBe` 2
      
      let track4 = track1 { Progress.trackTicks = 40 }
      Progress.getProgressScore track4 `shouldBe` 10

  describe "progressPercentage" $ do
    it "calculates percentage correctly" $ do
      let track1 = Progress.newProgressTrack "Test" Progress.Vow Progress.Dangerous
      Progress.progressPercentage track1 `shouldBe` 0.0
      
      let track2 = track1 { Progress.trackTicks = 20 }
      Progress.progressPercentage track2 `shouldBe` 50.0
      
      let track3 = track1 { Progress.trackTicks = 40 }
      Progress.progressPercentage track3 `shouldBe` 100.0

  describe "markProgress" $ do
    it "adds ticks based on rank" $ do
      let track = Progress.newProgressTrack "Test" Progress.Vow Progress.Dangerous
      updated <- Progress.markProgress track
      Progress.trackTicks updated `shouldBe` 8  
    
    it "does not exceed maximum ticks" $ do
      let track = (Progress.newProgressTrack "Test" Progress.Vow Progress.Dangerous)
            { Progress.trackTicks = 35 }
      updated <- Progress.markProgress track
      Progress.trackTicks updated `shouldBe` 40
    
    it "does not add ticks to completed tracks" $ do
      let track = (Progress.newProgressTrack "Test" Progress.Vow Progress.Dangerous)
            { Progress.trackCompleted = True, Progress.trackTicks = 10 }
      updated <- Progress.markProgress track
      Progress.trackTicks updated `shouldBe` 10

  describe "markProgressTicks" $ do
    it "adds specific number of ticks" $ do
      let track = Progress.newProgressTrack "Test" Progress.Vow Progress.Dangerous
      updated <- Progress.markProgressTicks track 5
      Progress.trackTicks updated `shouldBe` 5
    
    it "does not exceed maximum ticks" $ do
      let track = (Progress.newProgressTrack "Test" Progress.Vow Progress.Dangerous)
            { Progress.trackTicks = 35 }
      updated <- Progress.markProgressTicks track 10
      Progress.trackTicks updated `shouldBe` 40

  describe "evaluateProgressRoll" $ do
    it "returns StrongHit when score beats both challenge dice" $ do
      Progress.evaluateProgressRoll 5 3 4 `shouldBe` Dice.StrongHit
      Progress.evaluateProgressRoll 10 5 7 `shouldBe` Dice.StrongHit
    
    it "returns WeakHit when score beats only one challenge die" $ do
      Progress.evaluateProgressRoll 5 3 6 `shouldBe` Dice.WeakHit
      Progress.evaluateProgressRoll 7 8 4 `shouldBe` Dice.WeakHit
    
    it "returns Miss when score doesn't beat challenge dice" $ do
      Progress.evaluateProgressRoll 3 5 6 `shouldBe` Dice.Miss
      Progress.evaluateProgressRoll 4 4 4 `shouldBe` Dice.Miss

  describe "getRankExperienceValue" $ do
    it "returns correct experience values for ranks" $ do
      Progress.getRankExperienceValue Progress.Troublesome `shouldBe` 1
      Progress.getRankExperienceValue Progress.Dangerous `shouldBe` 2
      Progress.getRankExperienceValue Progress.Formidable `shouldBe` 3
      Progress.getRankExperienceValue Progress.Extreme `shouldBe` 4
      Progress.getRankExperienceValue Progress.Epic `shouldBe` 5

  describe "completeTrack" $ do
    it "marks track as completed" $ do
      let track = Progress.newProgressTrack "Test" Progress.Vow Progress.Dangerous
      completed <- Progress.completeTrack track
      Progress.trackCompleted completed `shouldBe` True

  describe "clearTrack" $ do
    it "resets track ticks and completion status" $ do
      let track = (Progress.newProgressTrack "Test" Progress.Vow Progress.Dangerous)
            { Progress.trackTicks = 20, Progress.trackCompleted = True }
      cleared <- Progress.clearTrack track
      Progress.trackTicks cleared `shouldBe` 0
      Progress.trackCompleted cleared `shouldBe` False
