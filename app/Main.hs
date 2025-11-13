module Main (main) where

import UI (runTui)
import MainLoop (runGameLoop)
import qualified System.Oracle as Oracle
import qualified System.AssetLoader as AssetLoader

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Data.Text as T
import System.Tui.Comm (GameOutput)

main :: IO ()
main = do
  
  inputChan <- newTChanIO :: IO (TChan T.Text)
  outputChan <- newTChanIO :: IO (TChan GameOutput)

  
  Oracle.initializeOracles ""
  _ <- AssetLoader.loadAllAssets

  
  _ <- forkIO $ runGameLoop outputChan inputChan

  
  runTui inputChan outputChan
