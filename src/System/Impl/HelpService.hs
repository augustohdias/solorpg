{-# LANGUAGE OverloadedStrings #-}
module System.Impl.HelpService (newHandle) where

import qualified System.HelpContract as Help
import qualified Data.Text as T
import System.FilePath ((</>), (<.>))
import System.Directory (doesFileExist)

newHandle :: FilePath -> IO Help.Handle
newHandle helpDir = do
  let handle = Help.Handle
        { Help.showHelp = loadAndShowHelp helpDir "general"
        , Help.showTopicHelp = loadAndShowHelp helpDir . topicToFileName
        , Help.parseTopic = parseTopic
        }
  return handle

-- | Carrega e exibe um arquivo de ajuda
loadAndShowHelp :: FilePath -> FilePath -> IO String
loadAndShowHelp helpDir fileName = do
  let path = helpDir </> fileName <.> "txt"
  exists <- doesFileExist path
  if exists
    then readFile path
    else return $ "Arquivo de ajuda não encontrado: " ++ path

-- | Mapeia Tópico para nome de arquivo
topicToFileName :: Help.Topic -> FilePath
topicToFileName topic = case topic of
  Help.Moves -> "moves"
  Help.Progress -> "progress"
  Help.Oracle -> "oracle"
  Help.Chaining -> "chaining"
  Help.Character -> "character"

-- | Parse de Tópico a partir de texto
parseTopic :: T.Text -> Maybe Help.Topic
parseTopic t = case T.toLower t of
  "moves" -> Just Help.Moves
  "progress" -> Just Help.Progress
  "oracle" -> Just Help.Oracle
  "chaining" -> Just Help.Chaining
  "character" -> Just Help.Character
  _ -> Nothing

