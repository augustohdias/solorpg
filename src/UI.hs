{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module UI (runTui) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter, vCenter)
import qualified Brick.Widgets.Edit as Ed
import qualified Brick.Focus as F
import qualified Graphics.Vty as Vty
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Data.Text.Zipper (clearZipper)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Data.List (intersperse)
import System.Tui.Comm (GameOutput(..), MessageType(..))
import qualified System.GameContextContract as GameContext
import Control.Monad.IO.Class (liftIO)
import Lens.Micro (Lens')
import System.Console.ANSI

-- Widget names
data Name = InputField | LogViewport | SystemViewport deriving (Eq, Ord, Show)

-- TUI State
data TuiState = TuiState
  { editor :: Ed.Editor T.Text Name
  , logs :: Vec.Vector T.Text           -- Narrative and game logs
  , systemMessages :: Vec.Vector T.Text -- System notifications
  , character :: Maybe GameContext.MainCharacter
  , inputChan :: TChan T.Text -- Channel to send commands to the game loop
  , viewportFocus :: F.FocusRing Name -- Focus ring for viewport scrolling
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
        , viewportFocus = F.focusRing [LogViewport, SystemViewport] -- Start with LogViewport focused
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
  _ <- customMainWithDefaultVty (Just eventChan) app initialState
  
  -- Clear screen and restore cursor position after TUI exits (vim-like behavior)
  clearScreen
  setCursorPosition 0 0

systemSheetWidth :: Int
systemSheetWidth = 40

charSheetWidth :: Int
charSheetWidth = 25

-- Main drawing function
drawTui :: TuiState -> [Widget Name]
drawTui ts = [ui]
  where
    ui = vBox [ header, mainContent, inputBox ]
    header = drawHeader (character ts) (viewportFocus ts)
    mainContent = hBox [ charSheet, logPanel, systemPanel ]
    charSheet = hLimit charSheetWidth $ borderWithLabel (str " Personagem ") $ padAll 1 $ drawCharacter (character ts)
    -- Log panel with word wrapping for long paragraphs
    -- Add spacing between log entries for better readability
    logPanel = withVScrollBars OnRight $ viewport LogViewport Vertical $ 
               vBox (intersperse (str " ") (map txtWrap (Vec.toList $ logs ts)))
    -- System panel on the right side, similar to Character block
    systemPanel = hLimit systemSheetWidth $ borderWithLabel (str " Sistema ") $ padAll 1 $
                  withVScrollBars OnRight $ viewport SystemViewport Vertical $
                  drawSystemMessages (systemMessages ts)
    inputBox = border $ vLimit 1 $ Ed.renderEditor (txt . T.concat) True (editor ts)

-- Draw header with scroll focus instruction
drawHeader :: Maybe GameContext.MainCharacter -> F.FocusRing Name -> Widget Name
drawHeader _ focusRing = 
  let focusIndicator = case F.focusGetCurrent focusRing of
        Just LogViewport -> "Log"
        Just SystemViewport -> "Sistema"
        _ -> "Nenhum"
      instruction = "Aperte <Tab> para mudar o foco do scroll | Foco atual: " ++ focusIndicator
  in hCenter $ str instruction

-- Draw system messages with word wrap and spacing
drawSystemMessages :: Vec.Vector T.Text -> Widget Name
drawSystemMessages msgs =
  let msgList = Vec.toList msgs
  in if null msgList
     then str "Nenhuma mensagem por enquanto."
     else vBox (intersperse (str " ") (map txtWrap msgList))

-- Draw character sheet
drawCharacter :: Maybe GameContext.MainCharacter -> Widget Name
drawCharacter Nothing = hCenter $ vCenter $ str "Sem personagem\ncarregado.\n\nDigite :help para ver\nas opções."
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

  -- Handle Tab key to switch between viewports (using Brick's FocusRing)
  VtyEvent (Vty.EvKey (Vty.KChar '\t') []) -> do
    st <- get
    put st { viewportFocus = F.focusNext (viewportFocus st) }
  
  -- Handle Shift+Tab to switch backwards
  VtyEvent (Vty.EvKey Vty.KBackTab []) -> do
    st <- get
    put st { viewportFocus = F.focusPrev (viewportFocus st) }
  
  -- Handle scroll events for the currently focused viewport
  VtyEvent (Vty.EvKey Vty.KUp []) -> do
    st <- get
    case F.focusGetCurrent (viewportFocus st) of
      Just viewportName -> vScrollBy (viewportScroll viewportName) (-1)
      Nothing -> return ()
  
  VtyEvent (Vty.EvKey Vty.KDown []) -> do
    st <- get
    case F.focusGetCurrent (viewportFocus st) of
      Just viewportName -> vScrollBy (viewportScroll viewportName) 1
      Nothing -> return ()
  
  VtyEvent (Vty.EvKey Vty.KPageUp []) -> do
    st <- get
    case F.focusGetCurrent (viewportFocus st) of
      Just viewportName -> vScrollBy (viewportScroll viewportName) (-10)
      Nothing -> return ()
  
  VtyEvent (Vty.EvKey Vty.KPageDown []) -> do
    st <- get
    case F.focusGetCurrent (viewportFocus st) of
      Just viewportName -> vScrollBy (viewportScroll viewportName) 10
      Nothing -> return ()

  -- Handle custom game events
  AppEvent (LogEntry msg msgType) -> do
    st <- get
    case msgType of
      SystemMessage -> do
        let newSysMsgs = systemMessages st `Vec.snoc` msg
        put st { systemMessages = newSysMsgs }
        -- Auto-scroll to bottom when new system message arrives
        vScrollToEnd (viewportScroll SystemViewport)
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
