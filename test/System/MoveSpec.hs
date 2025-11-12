{-# LANGUAGE OverloadedStrings #-}
module System.MoveSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified System.Move as Move
import qualified System.GameContext as GameContext
import qualified System.Dice as Dice
import qualified System.ConsequenceContract as Consequence

spec :: Spec
spec = describe "System.Move" $ do
  describe "statToText" $ do
    it "converts stats to text correctly" $ do
      Move.statToText Move.Iron `shouldBe` "iron"
      Move.statToText Move.Edge `shouldBe` "edge"
      Move.statToText Move.Heart `shouldBe` "heart"
      Move.statToText Move.Shadow `shouldBe` "shadow"
      Move.statToText Move.Wits `shouldBe` "wits"

  describe "parseStat" $ do
    it "parses valid stat names" $ do
      Move.parseStat "iron" `shouldBe` Just Move.Iron
      Move.parseStat "edge" `shouldBe` Just Move.Edge
      Move.parseStat "heart" `shouldBe` Just Move.Heart
      Move.parseStat "shadow" `shouldBe` Just Move.Shadow
      Move.parseStat "wits" `shouldBe` Just Move.Wits
    
    it "parses case-insensitive stat names" $ do
      Move.parseStat "IRON" `shouldBe` Just Move.Iron
      Move.parseStat "Edge" `shouldBe` Just Move.Edge
      Move.parseStat "  HEART  " `shouldBe` Just Move.Heart
    
    it "returns Nothing for invalid stat names" $ do
      Move.parseStat "invalid" `shouldBe` Nothing
      Move.parseStat "" `shouldBe` Nothing
      Move.parseStat "strength" `shouldBe` Nothing

  describe "parseMoveType" $ do
    it "parses fate moves" $ do
      Move.parseMoveType "paytheprice" `shouldBe` Just Consequence.PayThePrice
      Move.parseMoveType "askoracle" `shouldBe` Just Consequence.AskTheOracle
    
    it "parses adventure moves" $ do
      Move.parseMoveType "facedanger" `shouldBe` Just Consequence.FaceDanger
      Move.parseMoveType "gatherinformation" `shouldBe` Just Consequence.GatherInformation
      Move.parseMoveType "secureadvantage" `shouldBe` Just Consequence.SecureAdvantage
      Move.parseMoveType "undertakejourney" `shouldBe` Just Consequence.UndertakeJourney
      Move.parseMoveType "heal" `shouldBe` Just Consequence.Heal
      Move.parseMoveType "resupply" `shouldBe` Just Consequence.Resupply
      Move.parseMoveType "makecamp" `shouldBe` Just Consequence.MakeCamp
    
    it "parses combat moves" $ do
      Move.parseMoveType "enterthefray" `shouldBe` Just Consequence.EnterTheFray
      Move.parseMoveType "strike" `shouldBe` Just Consequence.Strike
      Move.parseMoveType "clash" `shouldBe` Just Consequence.Clash
      Move.parseMoveType "turnthetide" `shouldBe` Just Consequence.TurnTheTide
    
    it "parses quest moves" $ do
      Move.parseMoveType "swearironvow" `shouldBe` Just Consequence.SwearIronVow
      Move.parseMoveType "reachmilestone" `shouldBe` Just Consequence.ReachMilestone
      Move.parseMoveType "fulfillyourvow" `shouldBe` Just Consequence.FulfillYourVow
      Move.parseMoveType "forsakeyourvow" `shouldBe` Just Consequence.ForsakeYourVow
    
    it "parses case-insensitive move names" $ do
      Move.parseMoveType "FACEDANGER" `shouldBe` Just Consequence.FaceDanger
      Move.parseMoveType "FaceDanger" `shouldBe` Just Consequence.FaceDanger
      Move.parseMoveType "  facedanger  " `shouldBe` Just Consequence.FaceDanger
    
    it "returns Nothing for invalid move names" $ do
      Move.parseMoveType "invalid" `shouldBe` Nothing
      Move.parseMoveType "" `shouldBe` Nothing

  describe "executeMoveWithRoll" $ do
    it "executes move with given roll values" $ do
      let attrs = GameContext.Attributes 3 2 2 1 2
      let resources = GameContext.Resources 5 5 5 0 0
      
      consequences <- Move.executeMoveWithRoll 
        Consequence.FaceDanger 
        (Just Move.Iron) 
        5 
        (3, 4) 
        attrs 
        resources
      
      length consequences `shouldSatisfy` (> 0)
      head consequences `shouldSatisfy` (\c -> case c of
        Consequence.Narrative _ -> True
        _ -> False)
    
    it "handles moves without stat modifier" $ do
      let attrs = GameContext.Attributes 3 2 2 1 2
      let resources = GameContext.Resources 5 5 5 0 0
      
      consequences <- Move.executeMoveWithRoll 
        Consequence.FaceDanger 
        Nothing 
        5 
        (3, 4) 
        attrs 
        resources
      
      length consequences `shouldSatisfy` (> 0)
    
    it "calculates stat modifier correctly" $ do
      let attrs = GameContext.Attributes 3 2 2 1 2
      let resources = GameContext.Resources 5 5 5 0 0
      
      -- Strong hit: action die (5) + iron (3) = 8, beats both challenge dice (3, 4)
      consequences <- Move.executeMoveWithRoll 
        Consequence.FaceDanger 
        (Just Move.Iron) 
        5 
        (3, 4) 
        attrs 
        resources
      
      -- Should contain narrative with roll info
      any (\c -> case c of
            Consequence.Narrative txt -> T.isInfixOf "Action Die: 5" txt
            _ -> False) consequences `shouldBe` True
