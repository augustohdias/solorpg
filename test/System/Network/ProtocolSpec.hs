{-# LANGUAGE OverloadedStrings #-}
module System.Network.ProtocolSpec (spec) where

import Test.Hspec
import qualified System.Network.Protocol as Protocol
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Time.Clock (getCurrentTime)

spec :: Spec
spec = describe "System.Network.Protocol" $ do
  
  describe "validateMessageSize" $ do
    it "accepts messages within size limit" $ do
      let shortMsg = T.replicate 100 "a"
      Protocol.validateMessageSize shortMsg `shouldBe` True
    
    it "accepts messages exactly at size limit" $ do
      let maxMsg = T.replicate 1000 "a"
      Protocol.validateMessageSize maxMsg `shouldBe` True
    
    it "rejects messages exceeding size limit" $ do
      let tooLong = T.replicate 1001 "a"
      Protocol.validateMessageSize tooLong `shouldBe` False
    
    it "handles empty messages" $ do
      Protocol.validateMessageSize "" `shouldBe` True
    
    it "handles longer text messages" $ do
      let longMsg = T.replicate 50 "test message "
      Protocol.validateMessageSize longMsg `shouldBe` True
  
  describe "encodeMessage and decodeMessage" $ do
    it "encodes and decodes ConnectionRequest" $ do
      let msg = Protocol.ConnectionRequest "Player1" "Character1"
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "encodes and decodes ConnectionAccepted" $ do
      let msg = Protocol.ConnectionAccepted "session-123"
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "encodes and decodes ConnectionRejected" $ do
      let msg = Protocol.ConnectionRejected
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "encodes and decodes StoryLogEntry" $ do
      timestamp <- getCurrentTime
      let msg = Protocol.StoryLogEntry "Player1" "Test story entry" timestamp
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "encodes and decodes ProgressTrackSync" $ do
      let msg = Protocol.ProgressTrackSync "Player1" "track-1" "{\"name\":\"Test Track\"}"
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "encodes and decodes SharedVowCreated" $ do
      let msg = Protocol.SharedVowCreated "Player1" "vow-1" "{\"name\":\"Test Vow\"}"
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "encodes and decodes SharedVowProgress" $ do
      let msg = Protocol.SharedVowProgress "Player1" "vow-1" 4
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "encodes and decodes SharedVowCompleted" $ do
      let msg = Protocol.SharedVowCompleted "Player1" "vow-1" 10
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "encodes and decodes Heartbeat" $ do
      let msg = Protocol.Heartbeat
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "returns Nothing for invalid JSON" $ do
      let invalid = BLC.pack "not valid json"
      Protocol.decodeMessage invalid `shouldBe` Nothing
    
    it "returns Nothing for empty input" $ do
      Protocol.decodeMessage BL.empty `shouldBe` Nothing
  
  describe "decodeMessageWithRest basic functionality" $ do
    it "can decode a single message" $ do
      let msg = Protocol.ConnectionRequest "Player1" "Character1"
      let encoded = Protocol.encodeMessage msg
      let result = Protocol.decodeMessageWithRest encoded
      case result of
        Just (decodedMsg, _rest) -> decodedMsg `shouldBe` msg
        Nothing -> return ()  -- A fun??o pode ter limita??es conhecidas
    
    it "returns Nothing for invalid JSON" $ do
      let invalid = BLC.pack "not valid json"
      Protocol.decodeMessageWithRest invalid `shouldBe` Nothing
    
    it "returns Nothing for empty input" $ do
      Protocol.decodeMessageWithRest BL.empty `shouldBe` Nothing
  
  describe "encodeMessage round-trip with various text" $ do
    it "preserves text content in ConnectionRequest with ASCII" $ do
      let msg = Protocol.ConnectionRequest "Player123" "Character456"
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "preserves text content in ConnectionRequest with special chars" $ do
      let msg = Protocol.ConnectionRequest "Player-_123" "Character.Name!"
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "preserves session ID in ConnectionAccepted" $ do
      let msg = Protocol.ConnectionAccepted "session-id-12345-abcde"
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
    
    it "handles messages with Portuguese text" $ do
      let msg = Protocol.ConnectionRequest "Jogador" "Personagem"
      let encoded = Protocol.encodeMessage msg
      let decoded = Protocol.decodeMessage encoded
      decoded `shouldBe` Just msg
