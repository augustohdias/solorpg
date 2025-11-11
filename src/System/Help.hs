{-# LANGUAGE OverloadedStrings #-}
module System.Help
  ( Topic (..)
  , showHelp
  , showTopicHelp
  , parseTopic
  ) where

import qualified Data.Text as T
import System.FilePath ((</>), (<.>))
import System.Directory (doesFileExist)

data Topic = Moves | Progress | Oracle | Chaining | Character
  deriving (Eq, Show)

-- | Carrega e exibe ajuda geral
showHelp :: FilePath -> IO String
showHelp helpDir = loadAndShowHelp helpDir "general"

-- | Carrega e exibe ajuda de um tópico específico
showTopicHelp :: FilePath -> Topic -> IO String
showTopicHelp helpDir topic = loadAndShowHelp helpDir (topicToFileName topic)

-- | Parse de Tópico a partir de texto
parseTopic :: T.Text -> Maybe Topic
parseTopic t = case T.toLower t of
  "moves" -> Just Moves
  "progress" -> Just Progress
  "oracle" -> Just Oracle
  "chaining" -> Just Chaining
  "character" -> Just Character
  _ -> Nothing

-- | Carrega e exibe um arquivo de ajuda
loadAndShowHelp :: FilePath -> FilePath -> IO String
loadAndShowHelp helpDir fileName = do
  let path = helpDir </> fileName <.> "txt"
  exists <- doesFileExist path
  if exists
    then readFile path
    else return $ "Arquivo de ajuda não encontrado: " ++ path

-- | Mapeia Tópico para nome de arquivo
topicToFileName :: Topic -> FilePath
topicToFileName topic = case topic of
  Moves -> "moves"
  Progress -> "progress"
  Oracle -> "oracle"
  Chaining -> "chaining"
  Character -> "character"

