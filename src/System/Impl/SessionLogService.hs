{-# LANGUAGE OverloadedStrings #-}
module System.Impl.SessionLogService (newHandle) where

import qualified System.SessionLogContract as SessionLog
import qualified Data.Text as T
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)

newHandle :: IO SessionLog.Handle
newHandle = return $ SessionLog.Handle 
  { SessionLog.addToLog = addToLogImpl
  , SessionLog.loadLog = loadLogImpl 
  }
  where
    addToLogImpl :: T.Text -> [SessionLog.Log] -> IO [SessionLog.Log]
    addToLogImpl text existingLog = do
      currentTime <- getCurrentTime
      let timeStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
      let newLog = SessionLog.Log 
            { SessionLog.logText = text
            , SessionLog.timestamp = timeStr 
            }
      appendFile "session.log" (T.unpack text ++ "\n")
      return (newLog : existingLog)
    
    loadLogImpl :: FilePath -> IO [SessionLog.Log]
    loadLogImpl _filePath = do
      contents <- readFile "session.log"
      return [SessionLog.Log { SessionLog.logText = T.pack line, SessionLog.timestamp = "" } | line <- lines contents]