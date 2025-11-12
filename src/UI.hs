{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module UI (runTui) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Focus as F
import Brick.Widgets.Border
import Brick.Widgets.Center (centerLayer, hCenter, vCenter)
import qualified Brick.Widgets.Edit as Ed
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Char (digitToInt, isDigit)
import Data.List (find, intersperse)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Zipper (clearZipper)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as Vty
import Lens.Micro (Lens')
import qualified System.ConsequenceContract as Consequence
import System.Console.ANSI
import qualified System.GameContext as GameContext
import System.Tui.Comm
  ( ChoiceOptionPayload (..),
    ChoicePromptPayload (..),
    ChoiceSelectionPayload (..),
    GameOutput (..),
    MessageType (..),
  )

-- Widget names
data Name = InputField | LogViewport | SystemViewport | ChoicePopup deriving (Eq, Ord, Show)

-- | Holds decoded information for an individual choice option.
data ChoiceOptionState = ChoiceOptionState
  { optionPayload :: ChoiceOptionPayload,
    optionConsequences :: [Consequence.Consequence],
    optionDisabled :: Bool,
    optionError :: Maybe T.Text
  }

-- | Represents the popup currently shown to the player.
data ChoicePromptState = ChoicePromptState
  { choiceStatePayload :: ChoicePromptPayload,
    choiceStateOptions :: Vec.Vector ChoiceOptionState,
    choiceStateSelected :: Int,
    choiceStateError :: Maybe T.Text
  }

-- | Estado do prompt de aprovação de conexão
data ConnectionPromptState = ConnectionPromptState
  { connPromptPlayerName :: T.Text
  , connPromptCharacterName :: T.Text
  , connPromptSelected :: Int  -- 0 = Sim, 1 = Não
  }

-- TUI State
data TuiState = TuiState
  { editor :: Ed.Editor T.Text Name,
    logs :: Vec.Vector T.Text, -- Narrative and game logs
    systemMessages :: Vec.Vector T.Text, -- System notifications
    character :: Maybe GameContext.MainCharacter,
    inputChan :: TChan T.Text, -- Channel to send commands to the game loop
    viewportFocus :: F.FocusRing Name, -- Focus ring for viewport scrolling
    activeChoice :: Maybe ChoicePromptState, -- Popup prompting the user
    activeConnectionPrompt :: Maybe ConnectionPromptState, -- Prompt de aprovação de conexão
    multiplayerStatus :: Maybe (Bool, T.Text, T.Text), -- (isHost, ip, port) ou (isClient, host, port)
    connectedPlayers :: [T.Text] -- Lista de jogadores conectados
  }

choiceSelectedAttr, choiceDisabledAttr :: AttrName
choiceSelectedAttr = attrName "choiceSelected"
choiceDisabledAttr = attrName "choiceDisabled"

-- Brick App definition
app :: App TuiState GameOutput Name
app =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return (),
      appAttrMap =
        const $
          attrMap
            Vty.defAttr
            [ (choiceSelectedAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo),
              (choiceDisabledAttr, Vty.defAttr `Vty.withForeColor` Vty.brightBlack)
            ]
    }

-- Main entry point
runTui :: TChan T.Text -> TChan GameOutput -> IO ()
runTui inputChan' outputChan = do
  let initialState =
        TuiState
          { editor = Ed.editor InputField (Just 1) "",
            logs = Vec.empty,
            systemMessages = Vec.empty,
            character = Nothing,
            inputChan = inputChan',
            viewportFocus = F.focusRing [LogViewport, SystemViewport], -- Start with LogViewport focused
            activeChoice = Nothing,
            activeConnectionPrompt = Nothing,
            multiplayerStatus = Nothing,
            connectedPlayers = []
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
    threadDelay 2000000 -- 2 seconds
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
drawTui ts =
  case (activeChoice ts, activeConnectionPrompt ts) of
    (Just choiceState, _) -> [renderChoicePopup choiceState, baseUi]
    (_, Just connPrompt) -> [renderConnectionPrompt connPrompt, baseUi]
    _ -> [baseUi]
  where
    baseUi = vBox [header, mainContent, inputBox]
    header = drawHeader (character ts) (viewportFocus ts) (multiplayerStatus ts) (connectedPlayers ts)
    mainContent = hBox [leftPanel, logPanel, systemPanel]
    leftPanel = vBox [charSheet, cardsSheet]
    cardsSheet = hLimit charSheetWidth $ borderWithLabel (str " Cartas ") $ padAll 1 $ drawCards (character ts)
    charSheet = hLimit charSheetWidth $ borderWithLabel (str " Personagem ") $ padAll 1 $ drawCharacter (character ts)
    -- Log panel with word wrapping for long paragraphs
    -- Add spacing between log entries for better readability
    logPanel =
      withVScrollBars OnRight $
        viewport LogViewport Vertical $
          vBox (intersperse (str " ") (map txtWrap (Vec.toList $ logs ts)))
    -- System panel on the right side, similar to Character block
    systemPanel =
      hLimit systemSheetWidth $
        borderWithLabel (str " Sistema ") $
          padAll 1 $
            withVScrollBars OnRight $
              viewport SystemViewport Vertical $
                drawSystemMessages (systemMessages ts)
    inputBox = border $ vLimit 1 $ Ed.renderEditor (txt . T.concat) True (editor ts)

renderChoicePopup :: ChoicePromptState -> Widget Name
renderChoicePopup choiceState =
  centerLayer $
    borderWithLabel (txtWrap $ choicePromptTitle payload) $
      padAll 1 $
        vBox $
          [ txtWrap (choicePromptMessage payload),
            str " "
          ]
            ++ optionWidgets
            ++ errorWidgets
  where
    payload = choiceStatePayload choiceState
    options = choiceStateOptions choiceState
    optionWidgets
      | Vec.null options =
          [withAttr choiceDisabledAttr $ txtWrap "Nenhuma opção disponível."]
      | otherwise = concatMap renderOption $ zip [0 ..] (Vec.toList options)

    renderOption (idx, opt) =
      let label =
            T.pack (show (idx + 1)) <> ") " <> choiceOptionLabel (optionPayload opt)
          baseWidget = padLeft (Pad 2) $ txtWrap label
          styledWidget
            | optionDisabled opt = withAttr choiceDisabledAttr baseWidget
            | idx == choiceStateSelected choiceState = withAttr choiceSelectedAttr baseWidget
            | otherwise = baseWidget
          errorWidget =
            maybe
              []
              (\err -> [padLeft (Pad 4) $ withAttr choiceDisabledAttr (txtWrap err)])
              (optionError opt)
       in styledWidget : errorWidget

    errorWidgets =
      maybe [] (\err -> [str " ", withAttr choiceDisabledAttr (txtWrap err)]) (choiceStateError choiceState)

-- Draw header with scroll focus instruction and multiplayer status
drawHeader :: Maybe GameContext.MainCharacter -> F.FocusRing Name -> Maybe (Bool, T.Text, T.Text) -> [T.Text] -> Widget Name
drawHeader _ focusRing mpStatus players =
  let focusIndicator = case F.focusGetCurrent focusRing of
        Just LogViewport -> "Log"
        Just SystemViewport -> "Sistema"
        _ -> "Nenhum"
      instruction = "Aperte <Tab> para mudar o foco do scroll | Foco atual: " ++ focusIndicator
      mpInfo = case mpStatus of
        Just (True, ip, port) -> " | Host: " ++ T.unpack ip ++ ":" ++ T.unpack port
        Just (False, host, port) -> " | Conectado: " ++ T.unpack host ++ ":" ++ T.unpack port
        Nothing -> ""
      playersInfo = if null players then "" else " | Jogadores: " ++ T.unpack (T.intercalate ", " players)
   in hCenter $ str (instruction ++ mpInfo ++ playersInfo)

-- Render connection approval prompt
renderConnectionPrompt :: ConnectionPromptState -> Widget Name
renderConnectionPrompt prompt =
  centerLayer $
    borderWithLabel (str " Solicitação de Conexão ") $
      padAll 1 $
        vBox
          [ txtWrap $ T.concat ["Jogador ", connPromptPlayerName prompt, " (", connPromptCharacterName prompt, ") quer se conectar."],
            str " ",
            str "Aceitar conexão?",
            str " ",
            if connPromptSelected prompt == 0
              then withAttr choiceSelectedAttr $ padLeft (Pad 2) $ str "> Sim"
              else padLeft (Pad 2) $ str "  Sim",
            if connPromptSelected prompt == 1
              then withAttr choiceSelectedAttr $ padLeft (Pad 2) $ str "> Não"
              else padLeft (Pad 2) $ str "  Não",
            str " ",
            str "Use ↑/↓ para navegar, Enter para confirmar, Esc para cancelar"
          ]

-- Draw system messages with word wrap and spacing
drawSystemMessages :: Vec.Vector T.Text -> Widget Name
drawSystemMessages msgs =
  let msgList = Vec.toList msgs
   in if null msgList
        then str "Nenhuma mensagem por enquanto."
        else vBox (intersperse (str " ") (map txtWrap msgList))

-- Draw cards list
drawCards :: Maybe GameContext.MainCharacter -> Widget Name
drawCards Nothing = hCenter $ vCenter $ str "Você ainda não cartas disponíveis."
drawCards _ = vBox $ map buildCardDisplay placeholder
  where
    placeholder = ["Carta 1", "Carta 2", "Carta 3"]
    buildCardDisplay cardData = vBox [hCenter $ str $ T.unpack cardData]

-- Draw character sheet
drawCharacter :: Maybe GameContext.MainCharacter -> Widget Name
drawCharacter Nothing = hCenter $ vCenter $ str "Sem personagem\ncarregado.\n\nDigite :help para ver\nas opções."
drawCharacter (Just charData) =
  vBox
    [ hCenter $ str $ T.unpack (GameContext.name charData),
      hCenter $ str " ",
      str "Attributes",
      padLeft (Pad 2) $ drawAttrs (GameContext.attributes charData),
      str " ",
      str "Resources",
      padLeft (Pad 2) $ drawResources (GameContext.resources charData)
    ]

drawAttrs :: GameContext.Attributes -> Widget Name
drawAttrs attrs =
  vBox
    [ str $ "Iron:   " ++ show (GameContext.iron attrs),
      str $ "Edge:   " ++ show (GameContext.edge attrs),
      str $ "Heart:  " ++ show (GameContext.heart attrs),
      str $ "Shadow: " ++ show (GameContext.shadow attrs),
      str $ "Wits:   " ++ show (GameContext.wits attrs)
    ]

drawResources :: GameContext.Resources -> Widget Name
drawResources res =
  vBox
    [ str $ "Health:   " ++ show (GameContext.health res),
      str $ "Spirit:   " ++ show (GameContext.spirit res),
      str $ "Supply:   " ++ show (GameContext.supply res),
      str $ "Momentum: " ++ show (GameContext.momentum res),
      str $ "EXP:      " ++ show (GameContext.experience res)
    ]

-- Event handler
handleEvent :: BrickEvent Name GameOutput -> EventM Name TuiState ()
handleEvent event = case event of
  AppEvent (ChoicePrompt payload) ->
    handleChoicePromptEvent payload
  AppEvent (ConnectionRequestPrompt playerName charName) -> do
    st <- get
    let prompt = ConnectionPromptState
          { connPromptPlayerName = playerName
          , connPromptCharacterName = charName
          , connPromptSelected = 0  -- Começa selecionando "Sim"
          }
    put st {activeConnectionPrompt = Just prompt}
  AppEvent (HostInfo ip port) -> do
    st <- get
    put st {multiplayerStatus = Just (True, ip, port)}
  AppEvent (ConnectionStatus msg connected) -> do
    st <- get
    let newSysMsgs = systemMessages st `Vec.snoc` msg
    put st {systemMessages = newSysMsgs, multiplayerStatus = if connected then multiplayerStatus st else Nothing}
    vScrollToEnd (viewportScroll SystemViewport)
  AppEvent (PlayerList players) -> do
    st <- get
    put st {connectedPlayers = players}
  AppEvent (LogEntry msg msgType) -> do
    st <- get
    case msgType of
      SystemMessage -> do
        let newSysMsgs = systemMessages st `Vec.snoc` msg
        put st {systemMessages = newSysMsgs}
        vScrollToEnd (viewportScroll SystemViewport)
      NarrativeMessage -> do
        let newLogs = logs st `Vec.snoc` msg
        put st {logs = newLogs}
        vScrollToEnd (viewportScroll LogViewport)
  AppEvent (CharacterUpdate charData) -> do
    st <- get
    put st {character = Just charData}
    invalidateCache
  AppEvent GameEnd -> halt
  VtyEvent ev -> handleVtyEvent ev
  -- Ignore mouse events and others
  _ -> return ()

handleChoicePromptEvent :: ChoicePromptPayload -> EventM Name TuiState ()
handleChoicePromptEvent payload = do
  st <- get
  let newState = selectFirstEnabledOption (buildChoicePromptState payload)
  put st {activeChoice = Just newState}

buildChoicePromptState :: ChoicePromptPayload -> ChoicePromptState
buildChoicePromptState payload =
  let decoded = map decodeChoiceOption (choicePromptOptions payload)
      optionsVec = Vec.fromList (map fst decoded)
      optionErrors = mapMaybe snd decoded
      optionCountError =
        (["Nenhuma opção disponível para esta escolha." | Vec.null optionsVec])
      combinedErrors = optionErrors ++ optionCountError
      aggregatedError =
        case combinedErrors of
          [] -> Nothing
          errs -> Just (T.intercalate "\n" errs)
   in ChoicePromptState
        { choiceStatePayload = payload,
          choiceStateOptions = optionsVec,
          choiceStateSelected = 0,
          choiceStateError = aggregatedError
        }

decodeChoiceOption :: ChoiceOptionPayload -> (ChoiceOptionState, Maybe T.Text)
decodeChoiceOption payload =
  case Aeson.eitherDecodeStrict (TE.encodeUtf8 (choiceOptionConsequences payload)) of
    Left err ->
      ( ChoiceOptionState
          { optionPayload = payload,
            optionConsequences = [],
            optionDisabled = True,
            optionError = Just ("Erro ao decodificar consequências: " <> T.pack err)
          },
        Just $ choiceOptionLabel payload <> ": erro ao decodificar consequências (" <> T.pack err <> ")."
      )
    Right consequences ->
      ( ChoiceOptionState
          { optionPayload = payload,
            optionConsequences = consequences,
            optionDisabled = False,
            optionError = Nothing
          },
        Nothing
      )

selectFirstEnabledOption :: ChoicePromptState -> ChoicePromptState
selectFirstEnabledOption state =
  let options = choiceStateOptions state
      firstEnabled = Vec.findIndex (not . optionDisabled) options
   in state {choiceStateSelected = fromMaybe 0 firstEnabled}

handleVtyEvent :: Vty.Event -> EventM Name TuiState ()
handleVtyEvent ev = do
  st <- get
  case ev of
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> halt
    Vty.EvKey Vty.KEsc [] ->
      case (activeChoice st, activeConnectionPrompt st) of
        (Just _, _) -> cancelChoiceSelection
        (_, Just _) -> cancelConnectionPrompt
        _ -> halt
    _ ->
      case (activeChoice st, activeConnectionPrompt st) of
        (Just _, _) -> handleChoicePopupKey ev
        (_, Just _) -> handleConnectionPromptKey ev
        _ -> handleDefaultVtyEvent ev

handleChoicePopupKey :: Vty.Event -> EventM Name TuiState ()
handleChoicePopupKey ev = do
  st <- get
  case activeChoice st of
    Nothing -> return ()
    Just choiceState ->
      case ev of
        Vty.EvKey Vty.KEnter [] ->
          submitChoiceSelectionIndex (choiceStateSelected choiceState)
        Vty.EvKey Vty.KUp [] ->
          moveChoiceSelectionBy (-1)
        Vty.EvKey Vty.KDown [] ->
          moveChoiceSelectionBy 1
        Vty.EvKey Vty.KPageUp [] ->
          moveChoiceSelectionBy (-5)
        Vty.EvKey Vty.KPageDown [] ->
          moveChoiceSelectionBy 5
        Vty.EvKey (Vty.KChar ch) []
          | isDigit ch && ch /= '0' ->
              submitChoiceSelectionIndex (digitToInt ch - 1)
        _ -> return ()

-- Handle keyboard events for connection prompt
handleConnectionPromptKey :: Vty.Event -> EventM Name TuiState ()
handleConnectionPromptKey ev = do
  st <- get
  case activeConnectionPrompt st of
    Nothing -> return ()
    Just prompt ->
      case ev of
        Vty.EvKey Vty.KEnter [] ->
          submitConnectionResponse (connPromptSelected prompt == 0)
        Vty.EvKey Vty.KUp [] ->
          moveConnectionSelectionBy (-1)
        Vty.EvKey Vty.KDown [] ->
          moveConnectionSelectionBy 1
        Vty.EvKey (Vty.KChar 's') [] ->
          submitConnectionResponse True  -- 's' para Sim
        Vty.EvKey (Vty.KChar 'n') [] ->
          submitConnectionResponse False  -- 'n' para Não
        _ -> return ()

-- Move connection prompt selection
moveConnectionSelectionBy :: Int -> EventM Name TuiState ()
moveConnectionSelectionBy delta = do
  st <- get
  case activeConnectionPrompt st of
    Nothing -> return ()
    Just prompt -> do
      let current = connPromptSelected prompt
      let newSelection = max 0 (min 1 (current + delta))
      put st {activeConnectionPrompt = Just prompt {connPromptSelected = newSelection}}

-- Submit connection response (accept or reject)
submitConnectionResponse :: Bool -> EventM Name TuiState ()
submitConnectionResponse accept = do
  st <- get
  case activeConnectionPrompt st of
    Nothing -> return ()
    Just prompt -> do
      -- Envia comando para aceitar ou recusar conexão
      -- Por enquanto, vamos usar um comando especial que será processado pelo servidor
      -- TODO: Implementar comando específico para aprovação de conexão
      let command = if accept then ":accept" else ":reject"
      liftIO $ atomically $ writeTChan (inputChan st) command
      put st {activeConnectionPrompt = Nothing}

-- Cancel connection prompt
cancelConnectionPrompt :: EventM Name TuiState ()
cancelConnectionPrompt = do
  st <- get
  case activeConnectionPrompt st of
    Nothing -> return ()
    Just _ -> do
      -- Envia comando para recusar (cancelar = recusar)
      liftIO $ atomically $ writeTChan (inputChan st) ":reject"
      put st {activeConnectionPrompt = Nothing}

handleDefaultVtyEvent :: Vty.Event -> EventM Name TuiState ()
handleDefaultVtyEvent ev = case ev of
  Vty.EvKey Vty.KEnter [] -> submitCommand
  Vty.EvKey (Vty.KChar '\t') [] -> switchFocusNext
  Vty.EvKey Vty.KBackTab [] -> switchFocusPrev
  Vty.EvKey Vty.KUp [] -> scrollFocusedViewport (-1)
  Vty.EvKey Vty.KDown [] -> scrollFocusedViewport 1
  Vty.EvKey Vty.KPageUp [] -> scrollFocusedViewport (-10)
  Vty.EvKey Vty.KPageDown [] -> scrollFocusedViewport 10
  _ -> zoom editorL $ Ed.handleEditorEvent (VtyEvent ev)

submitCommand :: EventM Name TuiState ()
submitCommand = do
  st <- get
  let command = T.concat $ Ed.getEditContents $ editor st
  if T.null command
    then return ()
    else do
      liftIO $ atomically $ writeTChan (inputChan st) command
      let clearedEditor = Ed.applyEdit clearZipper (editor st)
      put st {editor = clearedEditor}

switchFocusNext :: EventM Name TuiState ()
switchFocusNext = do
  st <- get
  put st {viewportFocus = F.focusNext (viewportFocus st)}

switchFocusPrev :: EventM Name TuiState ()
switchFocusPrev = do
  st <- get
  put st {viewportFocus = F.focusPrev (viewportFocus st)}

scrollFocusedViewport :: Int -> EventM Name TuiState ()
scrollFocusedViewport delta = do
  st <- get
  case F.focusGetCurrent (viewportFocus st) of
    Just viewportName -> vScrollBy (viewportScroll viewportName) delta
    Nothing -> return ()

moveChoiceSelectionBy :: Int -> EventM Name TuiState ()
moveChoiceSelectionBy delta =
  updateActiveChoice (moveChoiceSelection delta)

updateActiveChoice :: (ChoicePromptState -> ChoicePromptState) -> EventM Name TuiState ()
updateActiveChoice f = do
  st <- get
  case activeChoice st of
    Nothing -> return ()
    Just cs -> put st {activeChoice = Just (f cs)}

moveChoiceSelection :: Int -> ChoicePromptState -> ChoicePromptState
moveChoiceSelection delta state
  | delta == 0 = state
  | Vec.null options = state
  | otherwise =
      let (result, moved) = iterateOnce (abs delta) state
       in if moved
            then result {choiceStateError = Nothing}
            else result
  where
    options = choiceStateOptions state
    direction = signum delta
    iterateOnce 0 st = (st, False)
    iterateOnce n st =
      case findNextEnabledIndex (choiceStateSelected st) direction options of
        Nothing -> (st, False)
        Just idx ->
          let (updated, _) = iterateOnce (n - 1) (st {choiceStateSelected = idx})
           in (updated, True)

findNextEnabledIndex :: Int -> Int -> Vec.Vector ChoiceOptionState -> Maybe Int
findNextEnabledIndex current direction options
  | Vec.null options = Nothing
  | direction == 0 = Just current
  | otherwise =
      let len = Vec.length options
          step idx = (idx + direction + len) `mod` len
          indices =
            take len $
              tail $
                iterate step current
       in find (\i -> not (optionDisabled (options Vec.! i))) indices

submitChoiceSelectionIndex :: Int -> EventM Name TuiState ()
submitChoiceSelectionIndex idx = do
  st <- get
  case activeChoice st of
    Nothing -> return ()
    Just choiceState -> do
      let options = choiceStateOptions choiceState
      case options Vec.!? idx of
        Nothing -> setChoiceError "Opção inválida."
        Just opt ->
          if optionDisabled opt
            then setChoiceError $ fromMaybe "Opção indisponível." (optionError opt)
            else do
              let payload =
                    ChoiceSelectionPayload
                      { choiceSelectionPromptId = choicePromptId (choiceStatePayload choiceState),
                        choiceSelectionSelectedIndex = choiceOptionIndex (optionPayload opt),
                        choiceSelectionLabel = choiceOptionLabel (optionPayload opt),
                        choiceSelectionConsequences = optionConsequences opt,
                        choiceSelectionCancelled = False
                      }
              sendChoiceCommand (inputChan st) payload
              put st {activeChoice = Nothing}

cancelChoiceSelection :: EventM Name TuiState ()
cancelChoiceSelection = do
  st <- get
  case activeChoice st of
    Nothing -> return ()
    Just choiceState -> do
      let options = choiceStateOptions choiceState
          selectedIdx = choiceStateSelected choiceState
          maybeOpt = options Vec.!? selectedIdx
          selectedIndex = maybe 0 (choiceOptionIndex . optionPayload) maybeOpt
          selectedLabel = maybe "" (choiceOptionLabel . optionPayload) maybeOpt
          payload =
            ChoiceSelectionPayload
              { choiceSelectionPromptId = choicePromptId (choiceStatePayload choiceState),
                choiceSelectionSelectedIndex = selectedIndex,
                choiceSelectionLabel = selectedLabel,
                choiceSelectionConsequences = [],
                choiceSelectionCancelled = True
              }
      sendChoiceCommand (inputChan st) payload
      put st {activeChoice = Nothing}

sendChoiceCommand :: TChan T.Text -> ChoiceSelectionPayload -> EventM Name TuiState ()
sendChoiceCommand chan payload = do
  let jsonText = TL.toStrict $ TLE.decodeUtf8 (Aeson.encode payload)
  liftIO $ atomically $ writeTChan chan (":choice " <> jsonText)

setChoiceError :: T.Text -> EventM Name TuiState ()
setChoiceError err =
  updateActiveChoice $ \st ->
    let combinedError =
          case choiceStateError st of
            Nothing -> err
            Just existing -> existing <> "\n" <> err
     in st {choiceStateError = Just combinedError}

-- Lens for the editor field
editorL :: Lens' TuiState (Ed.Editor T.Text Name)
editorL f s = (\e -> s {editor = e}) <$> f (editor s)
