module System.SessionLogContract (Handle (..), Log (..)) where

import qualified Data.Text as T

data Log = Log { logText :: T.Text, timestamp :: T.Text }
  deriving (Show)

data Handle = Handle 
    { addToLog :: T.Text -> [Log] -> IO [Log]
    , loadLog :: FilePath -> IO [Log]
    }