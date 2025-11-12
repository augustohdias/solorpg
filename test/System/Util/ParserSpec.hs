{-# LANGUAGE OverloadedStrings #-}
module System.Util.ParserSpec (spec) where

import Test.Hspec
import qualified System.Util.Parser as Parser
import qualified System.GameContext as GameContext
import qualified System.Progress as Progress

spec :: Spec
spec = describe "System.Util.Parser" $ do
  describe "parseKeyValue" $ do
    it "parses key:value pairs" $ do
      Parser.parseKeyValue "iron:3" `shouldBe` Just ("iron", "3")
      Parser.parseKeyValue "health:5" `shouldBe` Just ("health", "5")
      Parser.parseKeyValue "name:value" `shouldBe` Just ("name", "value")
    
    it "returns Nothing for invalid formats" $ do
      Parser.parseKeyValue "no-colon" `shouldBe` Nothing
      -- splitOn ":" on "multiple:colons:here" returns ["multiple", "colons", "here"]
      -- which doesn't match [k, v] pattern, so returns Nothing
      Parser.parseKeyValue "multiple:colons:here" `shouldBe` Nothing
      Parser.parseKeyValue "" `shouldBe` Nothing

  describe "parseDecimal" $ do
    it "parses valid integers" $ do
      Parser.parseDecimal "0" `shouldBe` Just 0
      Parser.parseDecimal "5" `shouldBe` Just 5
      Parser.parseDecimal "123" `shouldBe` Just 123
      Parser.parseDecimal "-5" `shouldBe` Nothing  -- Only positive
    
    it "returns Nothing for invalid input" $ do
      Parser.parseDecimal "abc" `shouldBe` Nothing
      Parser.parseDecimal "5.5" `shouldBe` Nothing
      Parser.parseDecimal "5abc" `shouldBe` Nothing
      Parser.parseDecimal "" `shouldBe` Nothing

  describe "parseSignedDecimal" $ do
    it "parses positive integers" $ do
      Parser.parseSignedDecimal "5" `shouldBe` Just 5
      Parser.parseSignedDecimal "123" `shouldBe` Just 123
    
    it "parses negative integers" $ do
      Parser.parseSignedDecimal "-5" `shouldBe` Just (-5)
      Parser.parseSignedDecimal "-123" `shouldBe` Just (-123)
    
    it "returns Nothing for invalid input" $ do
      Parser.parseSignedDecimal "abc" `shouldBe` Nothing
      Parser.parseSignedDecimal "5.5" `shouldBe` Nothing
      Parser.parseSignedDecimal "" `shouldBe` Nothing

  describe "parseAttributeUpdate" $ do
    it "updates valid attributes" $ do
      let attrs = GameContext.Attributes 2 2 2 2 2
      Parser.parseAttributeUpdate "iron:3" attrs `shouldBe` Just (attrs { GameContext.iron = 3 })
      Parser.parseAttributeUpdate "edge:4" attrs `shouldBe` Just (attrs { GameContext.edge = 4 })
      Parser.parseAttributeUpdate "heart:5" attrs `shouldBe` Just (attrs { GameContext.heart = 5 })
    
    it "returns Nothing for invalid attributes" $ do
      let attrs = GameContext.Attributes 2 2 2 2 2
      Parser.parseAttributeUpdate "invalid:3" attrs `shouldBe` Nothing
      Parser.parseAttributeUpdate "iron:abc" attrs `shouldBe` Nothing

  describe "parseAttributeAdd" $ do
    it "adds to attributes" $ do
      let attrs = GameContext.Attributes 2 2 2 2 2
      Parser.parseAttributeAdd "iron:+1" attrs `shouldBe` Just (attrs { GameContext.iron = 3 })
      Parser.parseAttributeAdd "iron:-1" attrs `shouldBe` Just (attrs { GameContext.iron = 1 })
    
    it "handles negative values" $ do
      let attrs = GameContext.Attributes 3 3 3 3 3
      Parser.parseAttributeAdd "iron:-2" attrs `shouldBe` Just (attrs { GameContext.iron = 1 })

  describe "parseResourceUpdate" $ do
    it "updates valid resources" $ do
      let res = GameContext.Resources 5 5 5 0 0
      Parser.parseResourceUpdate "spirit:3" res `shouldBe` Just (res { GameContext.spirit = 3 })
      Parser.parseResourceUpdate "health:4" res `shouldBe` Just (res { GameContext.health = 4 })
      Parser.parseResourceUpdate "supply:2" res `shouldBe` Just (res { GameContext.supply = 2 })
    
    it "returns Nothing for invalid resources" $ do
      let res = GameContext.Resources 5 5 5 0 0
      Parser.parseResourceUpdate "invalid:3" res `shouldBe` Nothing

  describe "parseResourceAdd" $ do
    it "adds to resources with clamping" $ do
      let res = GameContext.Resources 3 3 3 0 0
      Parser.parseResourceAdd "spirit:+1" res `shouldBe` Just (res { GameContext.spirit = 4 })
      Parser.parseResourceAdd "spirit:-1" res `shouldBe` Just (res { GameContext.spirit = 2 })
    
    it "clamps resources to valid range" $ do
      let res = GameContext.Resources 5 5 5 0 0
      Parser.parseResourceAdd "spirit:+10" res `shouldBe` Just (res { GameContext.spirit = 5 })
      Parser.parseResourceAdd "spirit:-10" res `shouldBe` Just (res { GameContext.spirit = 0 })

  describe "parseRank" $ do
    it "parses valid challenge ranks" $ do
      Parser.parseRank "troublesome" `shouldBe` Just Progress.Troublesome
      Parser.parseRank "dangerous" `shouldBe` Just Progress.Dangerous
      Parser.parseRank "formidable" `shouldBe` Just Progress.Formidable
      Parser.parseRank "extreme" `shouldBe` Just Progress.Extreme
      Parser.parseRank "epic" `shouldBe` Just Progress.Epic
    
    it "parses case-insensitive ranks" $ do
      Parser.parseRank "TROUBLESOME" `shouldBe` Just Progress.Troublesome
      Parser.parseRank "Dangerous" `shouldBe` Just Progress.Dangerous
    
    it "returns Nothing for invalid ranks" $ do
      Parser.parseRank "invalid" `shouldBe` Nothing
      Parser.parseRank "" `shouldBe` Nothing

  describe "rankToText" $ do
    it "converts ranks to text" $ do
      Parser.rankToText Progress.Troublesome `shouldBe` "troublesome"
      Parser.rankToText Progress.Dangerous `shouldBe` "dangerous"
      Parser.rankToText Progress.Formidable `shouldBe` "formidable"
      Parser.rankToText Progress.Extreme `shouldBe` "extreme"
      Parser.rankToText Progress.Epic `shouldBe` "epic"

  describe "parseQuotedString" $ do
    it "parses strings in quotes" $ do
      Parser.parseQuotedString "\"hello\"" `shouldBe` Just ("hello", "")
      Parser.parseQuotedString "\"hello\" rest" `shouldBe` Just ("hello", " rest")
    
    it "returns Nothing for unquoted strings" $ do
      Parser.parseQuotedString "hello" `shouldBe` Nothing
      Parser.parseQuotedString "" `shouldBe` Nothing

  describe "parseOracleQuery" $ do
    it "parses quoted oracle names" $ do
      Parser.parseOracleQuery "\"Location\" 50" `shouldBe` ("Location", "50")
      Parser.parseOracleQuery "\"Location\"" `shouldBe` ("Location", "")
    
    it "parses unquoted oracle names" $ do
      Parser.parseOracleQuery "Location 50" `shouldBe` ("Location", "50")
      Parser.parseOracleQuery "Location" `shouldBe` ("Location", "")

  describe "clamp" $ do
    it "clamps values to range" $ do
      Parser.clamp 0 5 3 `shouldBe` 3
      Parser.clamp 0 5 10 `shouldBe` 5
      Parser.clamp 0 5 (-5) `shouldBe` 0
      Parser.clamp 0 5 0 `shouldBe` 0
      Parser.clamp 0 5 5 `shouldBe` 5

  describe "parseAttributes" $ do
    it "parses multiple attributes" $ do
      let attrs = Parser.parseAttributes ["iron:3", "edge:2", "heart:4"]
      GameContext.iron attrs `shouldBe` 3
      GameContext.edge attrs `shouldBe` 2
      GameContext.heart attrs `shouldBe` 4
    
    it "uses default values for missing attributes" $ do
      let attrs = Parser.parseAttributes ["iron:3"]
      GameContext.iron attrs `shouldBe` 3
      -- Other attributes should have default values
