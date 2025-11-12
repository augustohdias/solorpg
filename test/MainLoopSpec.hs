{-# LANGUAGE OverloadedStrings #-}
module MainLoopSpec (spec) where

import Test.Hspec
import Data.Char (toLower)
import qualified Data.Text as T
import qualified MainLoop as ML
import qualified System.Action as Action

spec :: Spec
spec = describe "MainLoop" $ do
  describe "parseCommand" $ do
    it "parses commands starting with colon" $ do
      let (action, params) = ML.parseCommand ":r"
      action `shouldBe` Action.RollDice
      T.strip params `shouldBe` ""
    
    it "parses commands with parameters" $ do
      let (action, params) = ML.parseCommand ":move FaceDanger iron"
      action `shouldBe` Action.Move
      T.strip params `shouldBe` "FaceDanger iron"
    
    it "treats text without colon as story log" $ do
      let (action, params) = ML.parseCommand "This is a story entry"
      action `shouldBe` Action.AddStoryLog
      params `shouldBe` "This is a story entry"
    
    it "handles whitespace correctly" $ do
      let (action, params) = ML.parseCommand "  :r  "
      action `shouldBe` Action.RollDice
      T.strip params `shouldBe` ""

  describe "parseActionType" $ do
    it "parses dice roll commands" $ do
      ML.parseActionType ":r" `shouldBe` Action.RollDice
      ML.parseActionType ":roll" `shouldBe` Action.RollDice
    
    it "parses exit commands" $ do
      ML.parseActionType ":exit" `shouldBe` Action.Exit
      ML.parseActionType ":q" `shouldBe` Action.Exit
      ML.parseActionType ":quit" `shouldBe` Action.Exit
    
    it "parses character commands" $ do
      ML.parseActionType ":create" `shouldBe` Action.CreateCharacter
      ML.parseActionType ":load" `shouldBe` Action.LoadCharacter
      ML.parseActionType ":char" `shouldBe` Action.ShowCharacter
    
    it "parses move commands" $ do
      ML.parseActionType ":move" `shouldBe` Action.Move
      ML.parseActionType ":challenge" `shouldBe` Action.Challenge
    
    it "parses progress commands" $ do
      ML.parseActionType ":vow" `shouldBe` Action.SwearVow
      ML.parseActionType ":combat" `shouldBe` Action.CreateCombatTrack
      ML.parseActionType ":progress" `shouldBe` Action.MarkProgress
      ML.parseActionType ":fulfill" `shouldBe` Action.RollProgress
      ML.parseActionType ":tracks" `shouldBe` Action.ShowTracks
      ML.parseActionType ":abandon" `shouldBe` Action.AbandonTrack
    
    it "parses oracle commands" $ do
      ML.parseActionType ":oracle" `shouldBe` Action.Oracle
    
    it "parses help commands" $ do
      ML.parseActionType ":help" `shouldBe` Action.Help
    
    it "parses bond commands" $ do
      ML.parseActionType ":bond" `shouldBe` Action.Bond
    
    it "parses bonus commands" $ do
      ML.parseActionType ":bonus" `shouldBe` Action.AddBonusManually
    
    it "parses choice commands" $ do
      ML.parseActionType ":choice" `shouldBe` Action.ResolveChoice
    
    it "returns Unknown for invalid commands" $ do
      ML.parseActionType ":invalid" `shouldBe` Action.Unknown
      ML.parseActionType "invalid" `shouldBe` Action.Unknown
    
    it "handles case-insensitive commands (via toLower)" $ do
      
      
      ML.parseActionType ":r" `shouldBe` Action.RollDice
      ML.parseActionType ":exit" `shouldBe` Action.Exit
      
      
      ML.parseActionType (T.map toLower ":R") `shouldBe` Action.RollDice
      ML.parseActionType (T.map toLower ":EXIT") `shouldBe` Action.Exit
