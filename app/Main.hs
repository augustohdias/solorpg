module Main (main) where

import UI (runTui)
import MainLoop (runGameLoop)
import qualified System.Oracle as Oracle

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Data.Text as T
import System.Tui.Comm (GameOutput)

main :: IO ()
main = do
  -- Channels for communication between TUI and game engine
  inputChan <- newTChanIO :: IO (TChan T.Text)
  outputChan <- newTChanIO :: IO (TChan GameOutput)

  -- Initialize oracles (they use global cache)
  Oracle.initializeOracles "oracles"

  -- Fork the game loop thread
  _ <- forkIO $ runGameLoop outputChan inputChan

  -- Run the TUI on the main thread
  runTui inputChan outputChan
