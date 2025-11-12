{-# LANGUAGE OverloadedStrings #-}
module System.HelpSpec (spec) where

import Test.Hspec
import qualified System.Help as Help
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath ((</>))

spec :: Spec
spec = describe "System.Help" $ do
  describe "parseTopic" $ do
    it "parses valid topic names" $ do
      Help.parseTopic "moves" `shouldBe` Just Help.Moves
      Help.parseTopic "progress" `shouldBe` Just Help.Progress
      Help.parseTopic "oracle" `shouldBe` Just Help.Oracle
      Help.parseTopic "chaining" `shouldBe` Just Help.Chaining
      Help.parseTopic "character" `shouldBe` Just Help.Character
    
    it "parses topic names (case-insensitive via toLower)" $ do
      Help.parseTopic "MOVES" `shouldBe` Just Help.Moves
      Help.parseTopic "Moves" `shouldBe` Just Help.Moves
      Help.parseTopic "moves" `shouldBe` Just Help.Moves
    
    it "returns Nothing for invalid topic names" $ do
      Help.parseTopic "invalid" `shouldBe` Nothing
      Help.parseTopic "" `shouldBe` Nothing

  describe "showHelp" $ do
    it "loads general help file when it exists" $ do
      let testDir = "test_help_dir"
      createDirectoryIfMissing True testDir
      writeFile (testDir </> "general.txt") "General help content"
      
      result <- Help.showHelp testDir
      result `shouldContain` "General help content"
      
      
      removeFile (testDir </> "general.txt")
    
    it "returns error message when help file doesn't exist" $ do
      result <- Help.showHelp "nonexistent_dir"
      result `shouldContain` "não encontrado"

  describe "showTopicHelp" $ do
    it "loads topic-specific help file" $ do
      let testDir = "test_help_dir"
      createDirectoryIfMissing True testDir
      writeFile (testDir </> "moves.txt") "Moves help content"
      
      result <- Help.showTopicHelp testDir Help.Moves
      result `shouldContain` "Moves help content"
      
      
      removeFile (testDir </> "moves.txt")
