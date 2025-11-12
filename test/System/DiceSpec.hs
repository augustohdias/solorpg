{-# LANGUAGE OverloadedStrings #-}
module System.DiceSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import qualified System.Dice as Dice

spec :: Spec
spec = describe "System.Dice" $ do
  describe "parseDiceType" $ do
    it "parses valid dice types" $ do
      Dice.parseDiceType "2" `shouldBe` Just Dice.D2
      Dice.parseDiceType "4" `shouldBe` Just Dice.D4
      Dice.parseDiceType "6" `shouldBe` Just Dice.D6
      Dice.parseDiceType "8" `shouldBe` Just Dice.D8
      Dice.parseDiceType "10" `shouldBe` Just Dice.D10
      Dice.parseDiceType "12" `shouldBe` Just Dice.D12
      Dice.parseDiceType "20" `shouldBe` Just Dice.D20
      Dice.parseDiceType "100" `shouldBe` Just Dice.D100
    
    it "parses dice types correctly" $ do
      Dice.parseDiceType "6" `shouldBe` Just Dice.D6
      Dice.parseDiceType "10" `shouldBe` Just Dice.D10
      -- Note: parseDiceType expects just the number, not "D" prefix
    
    it "returns Nothing for invalid dice types" $ do
      Dice.parseDiceType "3" `shouldBe` Nothing
      Dice.parseDiceType "7" `shouldBe` Nothing
      Dice.parseDiceType "invalid" `shouldBe` Nothing
      Dice.parseDiceType "" `shouldBe` Nothing

  describe "parseSingleDice" $ do
    it "parses valid dice strings" $ do
      Dice.parseSingleDice "3d6" `shouldBe` Just (3, Dice.D6)
      Dice.parseSingleDice "2d10" `shouldBe` Just (2, Dice.D10)
      Dice.parseSingleDice "1d20" `shouldBe` Just (1, Dice.D20)
      Dice.parseSingleDice "5d4" `shouldBe` Just (5, Dice.D4)
    
    it "returns Nothing for invalid formats" $ do
      Dice.parseSingleDice "d6" `shouldBe` Nothing
      Dice.parseSingleDice "3" `shouldBe` Nothing
      Dice.parseSingleDice "3d" `shouldBe` Nothing
      Dice.parseSingleDice "invalid" `shouldBe` Nothing

  describe "parseDiceString" $ do
    it "parses single dice expressions" $ do
      Dice.parseDiceString "3d6" `shouldBe` Just [(3, Dice.D6)]
      Dice.parseDiceString "2d10" `shouldBe` Just [(2, Dice.D10)]
    
    it "parses multiple dice expressions" $ do
      Dice.parseDiceString "3d6,2d10" `shouldBe` Just [(3, Dice.D6), (2, Dice.D10)]
      Dice.parseDiceString "1d6,2d10,1d20" `shouldBe` Just [(1, Dice.D6), (2, Dice.D10), (1, Dice.D20)]
    
    it "returns Nothing for invalid expressions" $ do
      Dice.parseDiceString "invalid" `shouldBe` Nothing
      Dice.parseDiceString "3d6,invalid" `shouldBe` Nothing
      Dice.parseDiceString "" `shouldBe` Nothing

  describe "evaluateActionRoll" $ do
    it "returns StrongHit when action die beats both challenge dice" $ do
      Dice.evaluateActionRoll 6 3 4 `shouldBe` Dice.StrongHit
      Dice.evaluateActionRoll 10 5 7 `shouldBe` Dice.StrongHit
    
    it "returns WeakHit when action die beats only one challenge die" $ do
      Dice.evaluateActionRoll 5 3 6 `shouldBe` Dice.WeakHit
      Dice.evaluateActionRoll 7 8 4 `shouldBe` Dice.WeakHit
    
    it "returns Miss when action die doesn't beat challenge dice" $ do
      Dice.evaluateActionRoll 3 5 6 `shouldBe` Dice.Miss
      Dice.evaluateActionRoll 4 4 4 `shouldBe` Dice.Miss
      Dice.evaluateActionRoll 2 3 4 `shouldBe` Dice.Miss
    
    it "handles edge cases correctly" $ do
      Dice.evaluateActionRoll 5 5 5 `shouldBe` Dice.Miss
      Dice.evaluateActionRoll 6 5 5 `shouldBe` Dice.StrongHit
      Dice.evaluateActionRoll 5 5 4 `shouldBe` Dice.WeakHit

  describe "rollDice" $ do
    it "rolls dice within valid range" $ do
      result <- Dice.rollDice Dice.D6
      result `shouldSatisfy` (\x -> x >= 1 && x <= 6)
    
    it "rolls D10 within valid range" $ do
      result <- Dice.rollDice Dice.D10
      result `shouldSatisfy` (\x -> x >= 1 && x <= 10)
    
    it "rolls D20 within valid range" $ do
      result <- Dice.rollDice Dice.D20
      result `shouldSatisfy` (\x -> x >= 1 && x <= 20)

  describe "roll" $ do
    it "rolls valid dice expressions" $ do
      results <- Dice.roll "3d6"
      length results `shouldBe` 3
      all (\(dt, val) -> dt == Dice.D6 && val >= 1 && val <= 6) results `shouldBe` True
    
    it "rolls multiple dice types" $ do
      results <- Dice.roll "1d6,2d10"
      length results `shouldBe` 3
      let (d6Results, d10Results) = splitAt 1 results
      length d6Results `shouldBe` 1
      length d10Results `shouldBe` 2
    
    it "returns empty list for invalid input" $ do
      results <- Dice.roll "invalid"
      results `shouldBe` []

  describe "challengeRoll" $ do
    it "returns valid ChallengeResult" $ do
      result <- Dice.challengeRoll
      case result of
        Right cr -> do
          Dice.challengeActionDie cr `shouldSatisfy` (\x -> x >= 1 && x <= 6)
          Dice.challengeDie1 cr `shouldSatisfy` (\x -> x >= 1 && x <= 10)
          Dice.challengeDie2 cr `shouldSatisfy` (\x -> x >= 1 && x <= 10)
          Dice.challengeMatch cr `shouldBe` (Dice.challengeDie1 cr == Dice.challengeDie2 cr)
        Left _ -> expectationFailure "challengeRoll should return Right"

  describe "formatChallengeResult" $ do
    it "formats challenge result correctly" $ do
      let result = Dice.ChallengeResult 5 3 4 Dice.StrongHit False
      let formatted = Dice.formatChallengeResult 
            result 
            (\a c1 c2 msg -> "Action: " ++ show a ++ ", Challenge: " ++ show c1 ++ "," ++ show c2 ++ " - " ++ msg)
            "Strong Hit!"
            "Weak Hit!"
            "Miss!"
            "Match!"
      T.unpack formatted `shouldContain` "Action: 5"
      T.unpack formatted `shouldContain` "Strong Hit!"
