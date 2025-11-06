{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (intersperse)
import System.Tui.Comm (GameOutput(..), MessageType(..))
import qualified System.GameContextContract as GameContext
import Control.Monad.IO.Class (liftIO)
import Lens.Micro (Lens')

-- Widget names
data Name = InputField | LogViewport deriving (Eq, Ord, Show)

-- TUI State
data TuiState = TuiState
  { editor :: Ed.Editor T.Text Name
  , logs :: Vec.Vector T.Text           -- Narrative and game logs
  , systemMessages :: Vec.Vector T.Text -- System notifications
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
        , systemMessages = Vec.empty
        , character = Nothing
        , inputChan = inputChan'
        }
  
  eventChan <- newBChan 10
  
  -- Thread to forward game output to the TUI event loop
  void . forkIO . forever $ do
    output <- atomically $ readTChan outputChan
    -- Debug output (comment out in production)
    -- case output of
    --   LogEntry msg -> putStrLn $ "DEBUG TUI: Log - " ++ show msg
    --   CharacterUpdate _ -> putStrLn "DEBUG TUI: Character update"
    --   GameEnd -> putStrLn "DEBUG TUI: Game end"
    writeBChan eventChan output

  -- Thread to periodically request character sheet updates
  -- This ensures the UI stays in sync even if events are missed
  -- Runs continuously but silently (no logs generated)
  void . forkIO . forever $ do
    threadDelay 2000000  -- 2 seconds
    -- Send update request (won't spam logs since showCharacter doesn't log)
    atomically $ writeTChan inputChan' ":char"

  -- Run the TUI with custom event channel
  void $ fst <$> customMainWithDefaultVty (Just eventChan) app initialState

-- Main drawing function
drawTui :: TuiState -> [Widget Name]
drawTui ts = [ui]
  where
    ui = vBox [ mainContent, systemPanel, inputBox ]
    mainContent = hBox [ charSheet, logPanel ]
    charSheet = hLimit 30 $ borderWithLabel (str " Character ") $ padAll 1 $ drawCharacter (character ts)
    -- Log panel with word wrapping for long paragraphs
    -- Add spacing between log entries for better readability
    logPanel = withVScrollBars OnRight $ viewport LogViewport Vertical $ 
               vBox (intersperse (str " ") (map txtWrap (Vec.toList $ logs ts)))
    -- System panel takes full width (same as inputBox)
    systemPanel = borderWithLabel (str " System ") $ vLimit 2 $ padLeftRight 1 $ 
                  drawSystemMessages (systemMessages ts)
    inputBox = border $ vLimit 1 $ Ed.renderEditor (txt . T.concat) True (editor ts)

-- Draw system messages (only show last 2 messages with word wrap)
drawSystemMessages :: Vec.Vector T.Text -> Widget Name
drawSystemMessages msgs =
  let lastMsgs = Vec.toList $ Vec.drop (max 0 (Vec.length msgs - 2)) msgs
  in if null lastMsgs
     then txt " "
     else vBox (map txtWrap lastMsgs)

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
         -- Don't echo commands to logs - only show narrative and results
         let s' = s { editor = Ed.applyEdit clearZipper (editor s) }
         put s'

  -- Handle scroll events for the log viewport
  VtyEvent (Vty.EvKey Vty.KUp []) -> 
    vScrollBy (viewportScroll LogViewport) (-1)
  
  VtyEvent (Vty.EvKey Vty.KDown []) -> 
    vScrollBy (viewportScroll LogViewport) 1
  
  VtyEvent (Vty.EvKey Vty.KPageUp []) -> 
    vScrollBy (viewportScroll LogViewport) (-10)
  
  VtyEvent (Vty.EvKey Vty.KPageDown []) -> 
    vScrollBy (viewportScroll LogViewport) 10

  -- Handle custom game events
  AppEvent (LogEntry msg msgType) -> do
    st <- get
    case msgType of
      SystemMessage -> do
        let newSysMsgs = systemMessages st `Vec.snoc` msg
        put st { systemMessages = newSysMsgs }
      NarrativeMessage -> do
        let newLogs = logs st `Vec.snoc` msg
        put st { logs = newLogs }
        -- Auto-scroll to bottom when new log entry arrives
        vScrollToEnd (viewportScroll LogViewport)

  AppEvent (CharacterUpdate charData) -> do
    st <- get
    put st { character = Just charData }
    -- Force invalidate cache to ensure redraw
    invalidateCache

  AppEvent GameEnd -> halt

  -- Handle other Vty events - delegate to editor using zoom
  VtyEvent ev -> 
    zoom editorL $ Ed.handleEditorEvent (VtyEvent ev)
  
  -- Ignore mouse events
  _ -> return ()

-- Lens for the editor field
editorL :: Lens' TuiState (Ed.Editor T.Text Name)
editorL f s = (\e -> s { editor = e }) <$> f (editor s)
