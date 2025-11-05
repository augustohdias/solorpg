{-# LANGUAGE OverloadedStrings #-}
module UI (runTui) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter, vCenter)
import qualified Brick.Widgets.Edit as Ed
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Data.Text.Zipper (clearZipper)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import System.Tui.Comm (GameOutput(..))
import qualified System.GameContextContract as GameContext
import Control.Monad.IO.Class (liftIO)

-- Widget names
data Name = InputField | LogViewport deriving (Eq, Ord, Show)

-- TUI State
data TuiState = TuiState
  { editor :: Ed.Editor T.Text Name
  , logs :: Vec.Vector T.Text
  , character :: Maybe GameContext.MainCharacter
  , inputChan :: TChan T.Text -- Channel to send commands to the game loop
  }

-- Brick App definition
app :: App TuiState GameOutput Name
app = App
  { appDraw = drawTui
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const $ attrMap Vty.defAttr []
  }

-- Main entry point
runTui :: TChan T.Text -> TChan GameOutput -> IO ()
runTui inputChan' outputChan = do
  let initialState = TuiState
        { editor = Ed.editor InputField (Just 1) ""
        , logs = Vec.empty
        , character = Nothing
        , inputChan = inputChan'
        }
  
  eventChan <- newBChan 10
  
  -- Thread to forward game output to the TUI event loop
  void . forkIO . forever $ do
    output <- atomically $ readTChan outputChan
    writeBChan eventChan output

  void $ Brick.defaultMain app initialState

-- Main drawing function
drawTui :: TuiState -> [Widget Name]
drawTui ts = [ui]
  where
    ui = vBox [ mainContent, inputBox ]
    mainContent = hBox [ charSheet, logPanel ]
    charSheet = hLimit 30 $ borderWithLabel (str " Character ") $ padAll 1 $ drawCharacter (character ts)
    logPanel = withVScrollBars OnRight $ viewport LogViewport Vertical $ vBox (map txt (Vec.toList $ logs ts))
    inputBox = border $ vLimit 1 $ Ed.renderEditor (txt . T.concat) True (editor ts)

-- Draw character sheet
drawCharacter :: Maybe GameContext.MainCharacter -> Widget Name
drawCharacter Nothing = vCenter $ str "No character loaded."
drawCharacter (Just charData) =
  vBox [ hCenter $ str $ T.unpack (GameContext.name charData)
       , hCenter $ str " "
       , str "Attributes"
       , padLeft (Pad 2) $ drawAttrs (GameContext.attributes charData)
       , str " "
       , str "Resources"
       , padLeft (Pad 2) $ drawResources (GameContext.resources charData)
       ]

drawAttrs :: GameContext.Attributes -> Widget Name
drawAttrs attrs = vBox
  [ str $ "Iron:   " ++ show (GameContext.iron attrs)
  , str $ "Edge:   " ++ show (GameContext.edge attrs)
  , str $ "Heart:  " ++ show (GameContext.heart attrs)
  , str $ "Shadow: " ++ show (GameContext.shadow attrs)
  , str $ "Wits:   " ++ show (GameContext.wits attrs)
  ]

drawResources :: GameContext.Resources -> Widget Name
drawResources res = vBox
  [ str $ "Health:   " ++ show (GameContext.health res)
  , str $ "Spirit:   " ++ show (GameContext.spirit res)
  , str $ "Supply:   " ++ show (GameContext.supply res)
  , str $ "Momentum: " ++ show (GameContext.momentum res)
  , str $ "EXP:      " ++ show (GameContext.experience res)
  ]

-- Event handler
handleEvent :: BrickEvent Name GameOutput -> EventM Name TuiState ()
handleEvent event = case event of
  VtyEvent (Vty.EvKey Vty.KEsc []) -> halt
  VtyEvent (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) -> halt

  -- Handle Enter key to submit command
  VtyEvent (Vty.EvKey Vty.KEnter []) -> do
    s <- get
    let command = T.concat $ Ed.getEditContents $ editor s
    if T.null command
       then return ()
       else do
         liftIO $ atomically $ writeTChan (inputChan s) command
         let s' = s { editor = Ed.applyEdit clearZipper (editor s)
                    , logs = logs s `Vec.snoc` ("> " <> command) }
         put s'

  -- Handle scroll events for the log
  VtyEvent (Vty.EvKey Vty.KUp []) -> return ()
  VtyEvent (Vty.EvKey Vty.KDown []) -> return ()

  -- Handle custom game events
  AppEvent (LogEntry msg) -> do
    st <- get
    let newLogs = logs st `Vec.snoc` msg
    put st { logs = newLogs }

  AppEvent (CharacterUpdate charData) -> do
    st <- get
    put st { character = Just charData }

  AppEvent GameEnd -> halt

  -- Handle other Vty events - let Brick handle editor events automatically
  _ -> return ()
