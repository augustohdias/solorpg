module Main where

import UI (runTui)
import MainLoop (runGameLoop)
import qualified System.DiceContract as Dice
import qualified System.Impl.DiceService as DiceService
import qualified System.GameContextContract as GameContext
import qualified System.Impl.GameContextService as GameContextService
import qualified System.ActionContract as Action
import qualified System.Impl.ActionService as ActionService
import qualified System.MoveContract as Move
import qualified System.Impl.MoveService as MoveService
import qualified System.ProgressContract as Progress
import qualified System.Impl.ProgressService as ProgressService
import qualified System.OracleContract as Oracle
import qualified System.Impl.OracleService as OracleService
import qualified System.HelpContract as Help
import qualified System.Impl.HelpService as HelpService

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Data.Text as T
import System.Tui.Comm (GameOutput)

main :: IO ()
main = do
  -- Channels for communication between TUI and game engine
  inputChan <- newTChanIO :: IO (TChan T.Text)
  outputChan <- newTChanIO :: IO (TChan GameOutput)

  -- Initialize handles
  diceHandler <- DiceService.newHandle
  contextHandler <- GameContextService.newHandle
  progressHandler <- ProgressService.newHandle diceHandler
  oracleHandler <- OracleService.newHandle "oracles" diceHandler
  helpHandler <- HelpService.newHandle "help"
  moveHandler <- MoveService.newHandle diceHandler
  actionHandler <- ActionService.newHandle diceHandler contextHandler moveHandler progressHandler oracleHandler helpHandler outputChan

  -- Fork the game loop thread
  _ <- forkIO $ runGameLoop actionHandler inputChan outputChan

  -- Run the TUI on the main thread
  runTui inputChan outputChan
