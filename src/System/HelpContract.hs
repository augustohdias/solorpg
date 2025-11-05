{- | HelpContract - Sistema de ajuda do SoloRPG
     
     Fornece documentação interativa de todos os comandos e sistemas.
     
     Este módulo deve ser importado qualificado:
     > import qualified System.HelpContract as Help
-}
module System.HelpContract
  ( Handle (..)
  , Topic (..)
  )
where

import qualified Data.Text as T

data Topic = Moves | Progress | Oracle | Chaining | Character
  deriving (Eq, Show)

data Handle = Handle
  { showHelp      :: IO String
  , showTopicHelp :: Topic -> IO String
  , parseTopic    :: T.Text -> Maybe Topic
  }

