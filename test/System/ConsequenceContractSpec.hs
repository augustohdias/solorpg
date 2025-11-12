{-# LANGUAGE OverloadedStrings #-}
module System.ConsequenceContractSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import qualified System.ConsequenceContract as Consequence
import qualified System.GameContext as GameContext

spec :: Spec
spec = describe "System.ConsequenceContract" $ do
  describe "moveTypeToText" $ do
    it "converts move types to text" $ do
      Consequence.moveTypeToText Consequence.PayThePrice `shouldBe` "PayThePrice"
      Consequence.moveTypeToText Consequence.FaceDanger `shouldBe` "FaceDanger"
      Consequence.moveTypeToText Consequence.SwearIronVow `shouldBe` "SwearIronVow"
    
    it "handles all move types" $ do
      Consequence.moveTypeToText Consequence.AskTheOracle `shouldBe` "AskTheOracle"
      Consequence.moveTypeToText Consequence.EnterTheFray `shouldBe` "EnterTheFray"
      Consequence.moveTypeToText Consequence.EndurHarm `shouldBe` "EndureHarm"

  describe "Consequence types" $ do
    it "creates resource loss consequences" $ do
      let c1 = Consequence.LoseHealth 2
      let c2 = Consequence.LoseSpirit 1
      let c3 = Consequence.LoseSupply 3
      let c4 = Consequence.LoseMomentum 1
      
      -- Just verify they can be created
      c1 `shouldBe` Consequence.LoseHealth 2
      c2 `shouldBe` Consequence.LoseSpirit 1
    
    it "creates resource gain consequences" $ do
      let c1 = Consequence.GainHealth 2
      let c2 = Consequence.GainSpirit 1
      let c3 = Consequence.GainSupply 3
      let c4 = Consequence.GainMomentum 1
      
      c1 `shouldBe` Consequence.GainHealth 2
      c2 `shouldBe` Consequence.GainSpirit 1
    
    it "creates narrative consequences" $ do
      let c = Consequence.Narrative "Test narrative"
      c `shouldBe` Consequence.Narrative "Test narrative"
    
    it "creates trigger move consequences" $ do
      let c = Consequence.TriggerMove Consequence.FaceDanger
      c `shouldBe` Consequence.TriggerMove Consequence.FaceDanger
    
    it "creates trigger oracle consequences" $ do
      let c = Consequence.TriggerOracle "Location"
      c `shouldBe` Consequence.TriggerOracle "Location"
    
    it "creates player choice consequences" $ do
      let choice = Consequence.Choice 
            "Test choice" 
            [Consequence.Narrative "Result"]
      let c = Consequence.PlayerChoice [choice]
      c `shouldBe` Consequence.PlayerChoice [choice]
    
    it "creates bonus consequences" $ do
      let bonus = GameContext.ActiveBonus 
            GameContext.NextRoll 
            1 
            "Test bonus"
      let c = Consequence.AddBonus bonus
      c `shouldBe` Consequence.AddBonus bonus
    
    it "creates bond progress consequences" $ do
      let c = Consequence.MarkBondProgress
      c `shouldBe` Consequence.MarkBondProgress

  describe "Choice" $ do
    it "creates choices with description and consequences" $ do
      let choice = Consequence.Choice 
            "Choose this" 
            [Consequence.GainMomentum 1, Consequence.Narrative "You chose"]
      
      Consequence.choiceDescription choice `shouldBe` "Choose this"
      length (Consequence.choiceConsequences choice) `shouldBe` 2
