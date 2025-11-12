{-# LANGUAGE LambdaCase #-}
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
import Control.Monad (forever, void, when)
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
import qualified System.AssetLoader as AssetLoader
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

data Name = InputField | LogViewport | SystemViewport | CardsViewport | ChoicePopup | AssetExplorePopup | AssetViewPopup deriving (Eq, Ord, Show)

data ChoiceOptionState = ChoiceOptionState
  { optionPayload :: ChoiceOptionPayload,
    optionConsequences :: [Consequence.Consequence],
    optionDisabled :: Bool,
    optionError :: Maybe T.Text
  }

data ChoicePromptState = ChoicePromptState
  { choiceStatePayload :: ChoicePromptPayload,
    choiceStateOptions :: Vec.Vector ChoiceOptionState,
    choiceStateSelected :: Int,
    choiceStateError :: Maybe T.Text
  }

data ConnectionPromptState = ConnectionPromptState
  { connPromptPlayerName :: T.Text,
    connPromptCharacterName :: T.Text,
    connPromptSelected :: Int
  }

data AssetExploreState = AssetExploreState
  { exploreAssets :: Vec.Vector GameContext.Asset,
    exploreSelected :: Int
  }

data AssetViewState = AssetViewState
  { viewAsset :: GameContext.Asset,
    viewSkillSelected :: Int,
    viewPlayerAsset :: Maybe GameContext.PlayerAsset
  }

data TuiState = TuiState
  { editor :: Ed.Editor T.Text Name,
    logs :: Vec.Vector T.Text,
    systemMessages :: Vec.Vector T.Text,
    character :: Maybe GameContext.MainCharacter,
    inputChan :: TChan T.Text,
    viewportFocus :: F.FocusRing Name,
    activeChoice :: Maybe ChoicePromptState,
    activeConnectionPrompt :: Maybe ConnectionPromptState,
    activeAssetExplore :: Maybe AssetExploreState,
    activeAssetView :: Maybe AssetViewState,
    cardsSelection :: Int,
    multiplayerStatus :: Maybe (Bool, T.Text, T.Text),
    connectedPlayers :: [T.Text]
  }

choiceSelectedAttr, choiceDisabledAttr :: AttrName
choiceSelectedAttr = attrName "choiceSelected"
choiceDisabledAttr = attrName "choiceDisabled"

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

runTui :: TChan T.Text -> TChan GameOutput -> IO ()
runTui inputChan' outputChan = do
  let initialState =
        TuiState
          { editor = Ed.editor InputField (Just 1) "",
            logs = Vec.empty,
            systemMessages = Vec.empty,
            character = Nothing,
            inputChan = inputChan',
            viewportFocus = F.focusRing [LogViewport, SystemViewport, CardsViewport],
            activeChoice = Nothing,
            activeConnectionPrompt = Nothing,
            activeAssetExplore = Nothing,
            activeAssetView = Nothing,
            cardsSelection = 0,
            multiplayerStatus = Nothing,
            connectedPlayers = []
          }

  eventChan <- newBChan 10

  void . forkIO . forever $ do
    output <- atomically $ readTChan outputChan

    writeBChan eventChan output

  void . forkIO . forever $ do
    threadDelay 2000000

    atomically $ writeTChan inputChan' ":char"

  _ <- customMainWithDefaultVty (Just eventChan) app initialState

  clearScreen
  setCursorPosition 0 0

systemSheetWidth :: Int
systemSheetWidth = 40

charSheetWidth :: Int
charSheetWidth = 25

drawTui :: TuiState -> [Widget Name]
drawTui ts =
  case (activeChoice ts, activeConnectionPrompt ts, activeAssetExplore ts, activeAssetView ts) of
    (Just choiceState, _, _, _) -> [renderChoicePopup choiceState, baseUi]
    (_, Just connPrompt, _, _) -> [renderConnectionPrompt connPrompt, baseUi]
    (_, _, Just exploreState, _) -> [renderAssetExplorePopup exploreState, baseUi]
    (_, _, _, Just viewState) -> [renderAssetViewPopup viewState, baseUi]
    _ -> [baseUi]
  where
    baseUi = vBox [header, mainContent, inputBox]
    header = drawHeader (character ts) (viewportFocus ts) (multiplayerStatus ts) (connectedPlayers ts)
    mainContent = hBox [leftPanel, logPanel, systemPanel]
    leftPanel = vBox [charSheet, cardsSheet]
    cardsSheet = vLimit charSheetWidth $ hLimit charSheetWidth $ borderWithLabel (str " Cartas ") $ padAll 1 $ drawCards (character ts) (viewportFocus ts) (cardsSelection ts)
    charSheet = vLimit charSheetWidth $ hLimit charSheetWidth $ borderWithLabel (str " Personagem ") $ padAll 1 $ drawCharacter (character ts)

    logPanel =
      withVScrollBars OnRight $
        viewport LogViewport Vertical $
          vBox (intersperse (str " ") (map txtWrap (Vec.toList $ logs ts)))

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

drawHeader :: Maybe GameContext.MainCharacter -> F.FocusRing Name -> Maybe (Bool, T.Text, T.Text) -> [T.Text] -> Widget Name
drawHeader _ focusRing mpStatus players =
  let focusIndicator = case F.focusGetCurrent focusRing of
        Just LogViewport -> "Log"
        Just SystemViewport -> "Sistema"
        Just CardsViewport -> "Cartas"
        _ -> "Nenhum"
      instruction = "Aperte <Tab> para mudar o foco do scroll | Foco atual: " ++ focusIndicator
      mpInfo = case mpStatus of
        Just (True, ip, port) -> " | Host: " ++ T.unpack ip ++ ":" ++ T.unpack port
        Just (False, host, port) -> " | Conectado: " ++ T.unpack host ++ ":" ++ T.unpack port
        Nothing -> ""
      playersInfo = if null players then "" else " | Jogadores: " ++ T.unpack (T.intercalate ", " players)
   in hCenter $ str (instruction ++ mpInfo ++ playersInfo)

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

drawSystemMessages :: Vec.Vector T.Text -> Widget Name
drawSystemMessages msgs =
  let msgList = Vec.toList msgs
   in if null msgList
        then str "Nenhuma mensagem por enquanto."
        else vBox (intersperse (str " ") (map txtWrap msgList))

drawCards :: Maybe GameContext.MainCharacter -> F.FocusRing Name -> Int -> Widget Name
drawCards Nothing _ _ = hCenter $ vCenter $ str "Nenhum personagem carregado."
drawCards (Just char) focusRing selectedIdx =
  if null playerAssets
    then hCenter $ vCenter $ str "Nenhum asset adquirido."
    else vBox $ zipWith (curry (buildAssetDisplay isFocused selectedIdx)) [0 ..] playerAssets
  where
    playerAssets = GameContext.assets char
    isFocused = F.focusGetCurrent focusRing == Just CardsViewport

    buildAssetDisplay :: Bool -> Int -> (Int, GameContext.PlayerAsset) -> Widget Name
    buildAssetDisplay focused selIdx (idx, playerAsset) =
      let isSelected = focused && idx == selIdx
          assetName = GameContext.playerAssetName playerAsset
          widget =
            if isSelected
              then withAttr choiceSelectedAttr $ str $ "> " <> T.unpack assetName
              else str $ "  " <> T.unpack assetName
       in widget

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

handleEvent :: BrickEvent Name GameOutput -> EventM Name TuiState ()
handleEvent event = case event of
  AppEvent (ChoicePrompt payload) ->
    handleChoicePromptEvent payload
  AppEvent (ConnectionRequestPrompt playerName charName) -> do
    st <- get
    let prompt =
          ConnectionPromptState
            { connPromptPlayerName = playerName,
              connPromptCharacterName = charName,
              connPromptSelected = 0
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
  AppEvent (AssetExploreRequest assets) -> do
    st <- get
    let exploreState =
          AssetExploreState
            { exploreAssets = Vec.fromList assets,
              exploreSelected = 0
            }
    put st {activeAssetExplore = Just exploreState}
  AppEvent (AssetViewRequest asset) -> do
    st <- get
    let viewState =
          AssetViewState
            { viewAsset = asset,
              viewSkillSelected = 0,
              viewPlayerAsset = Nothing
            }
    put st {activeAssetView = Just viewState}
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
    Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] -> return ()
    Vty.EvKey Vty.KEsc [] ->
      case (activeChoice st, activeConnectionPrompt st, activeAssetExplore st, activeAssetView st) of
        (Just _, _, _, _) -> cancelChoiceSelection
        (_, Just _, _, _) -> cancelConnectionPrompt
        (_, _, Just _, _) -> cancelAssetExplore
        (_, _, _, Just _) -> cancelAssetView
        _ -> return ()
    _ ->
      case (activeChoice st, activeConnectionPrompt st, activeAssetExplore st, activeAssetView st) of
        (Just _, _, _, _) -> handleChoicePopupKey ev
        (_, Just _, _, _) -> handleConnectionPromptKey ev
        (_, _, Just _, _) -> handleAssetExploreKey ev
        (_, _, _, Just _) -> handleAssetViewKey ev
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
          submitConnectionResponse True
        Vty.EvKey (Vty.KChar 'n') [] ->
          submitConnectionResponse False
        _ -> return ()

moveConnectionSelectionBy :: Int -> EventM Name TuiState ()
moveConnectionSelectionBy delta = do
  st <- get
  case activeConnectionPrompt st of
    Nothing -> return ()
    Just prompt -> do
      let current = connPromptSelected prompt
      let newSelection = max 0 (min 1 (current + delta))
      put st {activeConnectionPrompt = Just prompt {connPromptSelected = newSelection}}

submitConnectionResponse :: Bool -> EventM Name TuiState ()
submitConnectionResponse accept = do
  st <- get
  case activeConnectionPrompt st of
    Nothing -> return ()
    Just _ -> do
      let command = if accept then ":accept" else ":reject"
      liftIO $ atomically $ writeTChan (inputChan st) command
      put st {activeConnectionPrompt = Nothing}

cancelConnectionPrompt :: EventM Name TuiState ()
cancelConnectionPrompt = do
  st <- get
  case activeConnectionPrompt st of
    Nothing -> return ()
    Just _ -> do
      liftIO $ atomically $ writeTChan (inputChan st) ":reject"
      put st {activeConnectionPrompt = Nothing}

handleDefaultVtyEvent :: Vty.Event -> EventM Name TuiState ()
handleDefaultVtyEvent ev = do
  st <- get
  case ev of
    Vty.EvKey (Vty.KChar '\t') [] -> switchFocusNext
    Vty.EvKey Vty.KBackTab [] -> switchFocusPrev
    Vty.EvKey Vty.KUp [] ->
      if F.focusGetCurrent (viewportFocus st) == Just CardsViewport
        then moveCardsSelection (-1)
        else scrollFocusedViewport (-1)
    Vty.EvKey Vty.KDown [] ->
      if F.focusGetCurrent (viewportFocus st) == Just CardsViewport
        then moveCardsSelection 1
        else scrollFocusedViewport 1
    Vty.EvKey Vty.KPageUp [] ->
      if F.focusGetCurrent (viewportFocus st) == Just CardsViewport
        then moveCardsSelection (-5)
        else scrollFocusedViewport (-10)
    Vty.EvKey Vty.KPageDown [] ->
      if F.focusGetCurrent (viewportFocus st) == Just CardsViewport
        then moveCardsSelection 5
        else scrollFocusedViewport 10
    Vty.EvKey Vty.KEnter [] ->
      if F.focusGetCurrent (viewportFocus st) == Just CardsViewport
        then openSelectedPlayerAsset
        else submitCommand
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

cancelAssetExplore :: EventM Name TuiState ()
cancelAssetExplore = do
  st <- get
  put st {activeAssetExplore = Nothing}

cancelAssetView :: EventM Name TuiState ()
cancelAssetView = do
  st <- get
  put st {activeAssetView = Nothing}

handleAssetExploreKey :: Vty.Event -> EventM Name TuiState ()
handleAssetExploreKey ev = do
  st <- get
  case activeAssetExplore st of
    Nothing -> return ()
    Just exploreState ->
      case ev of
        Vty.EvKey Vty.KEnter [] -> do
          let assets = exploreAssets exploreState
          let selectedIdx = exploreSelected exploreState
          when (selectedIdx >= 0 && selectedIdx < Vec.length assets) $ do
            let selectedAsset = assets Vec.! selectedIdx
            let viewState =
                  AssetViewState
                    { viewAsset = selectedAsset,
                      viewSkillSelected = 0,
                      viewPlayerAsset = Nothing
                    }
            put st {activeAssetExplore = Nothing, activeAssetView = Just viewState}
        Vty.EvKey Vty.KUp [] ->
          moveAssetExploreSelection (-1)
        Vty.EvKey Vty.KDown [] ->
          moveAssetExploreSelection 1
        Vty.EvKey Vty.KPageUp [] ->
          moveAssetExploreSelection (-5)
        Vty.EvKey Vty.KPageDown [] ->
          moveAssetExploreSelection 5
        _ -> return ()

handleAssetViewKey :: Vty.Event -> EventM Name TuiState ()
handleAssetViewKey ev = do
  st <- get
  case activeAssetView st of
    Nothing -> return ()
    Just _ ->
      case ev of
        Vty.EvKey Vty.KUp [] ->
          moveAssetViewSkillSelection (-1)
        Vty.EvKey Vty.KDown [] ->
          moveAssetViewSkillSelection 1
        Vty.EvKey Vty.KEnter [] ->
          showSelectedSkillDescription
        _ -> return ()

moveAssetExploreSelection :: Int -> EventM Name TuiState ()
moveAssetExploreSelection delta = do
  st <- get
  case activeAssetExplore st of
    Nothing -> return ()
    Just exploreState -> do
      let assets = exploreAssets exploreState
      let currentIdx = exploreSelected exploreState
      let maxIdx = Vec.length assets - 1
      let newIdx = max 0 (min maxIdx (currentIdx + delta))
      let newState = exploreState {exploreSelected = newIdx}
      put st {activeAssetExplore = Just newState}

moveAssetViewSkillSelection :: Int -> EventM Name TuiState ()
moveAssetViewSkillSelection delta = do
  st <- get
  case activeAssetView st of
    Nothing -> return ()
    Just viewState -> do
      let asset = viewAsset viewState
      let currentIdx = viewSkillSelected viewState
      let maxIdx = length (GameContext.assetSkills asset) - 1
      let newIdx = max 0 (min maxIdx (currentIdx + delta))
      let newState = viewState {viewSkillSelected = newIdx}
      put st {activeAssetView = Just newState}

moveCardsSelection :: Int -> EventM Name TuiState ()
moveCardsSelection delta = do
  st <- get
  case character st of
    Nothing -> return ()
    Just char -> do
      let playerAssets = GameContext.assets char
      let currentIdx = cardsSelection st
      let maxIdx = length playerAssets - 1
      let newIdx = max 0 (min maxIdx (currentIdx + delta))
      put st {cardsSelection = newIdx}

openSelectedPlayerAsset :: EventM Name TuiState ()
openSelectedPlayerAsset = do
  st <- get
  case character st of
    Nothing -> return ()
    Just char -> do
      let playerAssets = GameContext.assets char
      let selectedIdx = cardsSelection st
      when (selectedIdx >= 0 && selectedIdx < length playerAssets) $ do
        let playerAsset = playerAssets !! selectedIdx

        liftIO (findPlayerAssetByName (GameContext.playerAssetName playerAsset)) >>= \case
          Nothing -> return ()
          Just fullAsset -> do
            let viewState =
                  AssetViewState
                    { viewAsset = fullAsset,
                      viewSkillSelected = 0,
                      viewPlayerAsset = Just playerAsset
                    }
            put st {activeAssetView = Just viewState}

findPlayerAssetByName :: T.Text -> IO (Maybe GameContext.Asset)
findPlayerAssetByName assetName = do
  find (\asset -> GameContext.assetName asset == assetName) <$> getAvailableAssets
  where
    getAvailableAssets = AssetLoader.getAvailableAssets

showSelectedSkillDescription :: EventM Name TuiState ()
showSelectedSkillDescription = do
  st <- get
  case activeAssetView st of
    Nothing -> return ()
    Just viewState -> do
      let asset = viewAsset viewState
      let selectedIdx = viewSkillSelected viewState
      let skills = GameContext.assetSkills asset
      when (selectedIdx >= 0 && selectedIdx < length skills) $ do
        let selectedSkill = skills !! selectedIdx
        let skillName = GameContext.skillName selectedSkill
        let assetName = GameContext.assetName asset

        let command = ":showskill \"" <> assetName <> "\" \"" <> skillName <> "\""
        liftIO $ atomically $ writeTChan (inputChan st) command

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

editorL :: Lens' TuiState (Ed.Editor T.Text Name)
editorL f s = (\e -> s {editor = e}) <$> f (editor s)

renderAssetExplorePopup :: AssetExploreState -> Widget Name
renderAssetExplorePopup exploreState =
  centerLayer $
    borderWithLabel (str " Explorar Assets ") $
      padAll 1 $
        vBox $
          [ str "Use ↑/↓ para navegar, ENTER para ver detalhes, ESC para fechar",
            str " "
          ]
            ++ assetWidgets
  where
    assets = exploreAssets exploreState
    selectedIdx = exploreSelected exploreState
    assetWidgets = Vec.toList $ Vec.imap renderAssetItem assets

    renderAssetItem :: Int -> GameContext.Asset -> Widget Name
    renderAssetItem idx asset =
      let isSelected = idx == selectedIdx
          assetText = GameContext.assetName asset <> " (" <> assetTypeToText (GameContext.assetType asset) <> ")"
          widget =
            if isSelected
              then withAttr choiceSelectedAttr $ padLeft (Pad 2) $ str $ "> " <> T.unpack assetText
              else padLeft (Pad 4) $ str $ T.unpack assetText
       in widget

renderAssetViewPopup :: AssetViewState -> Widget Name
renderAssetViewPopup viewState =
  centerLayer $
    hLimit 80 $
      borderWithLabel (str $ " " <> T.unpack (GameContext.assetName asset) <> " ") $
        padAll 1 $
          vBox $
            [ str $ "Tipo: " <> T.unpack (assetTypeToText (GameContext.assetType asset)),
              str " "
            ]
              ++ skillWidgets
              ++ [ str " ",
                   str "* = habilidade habilitada, - = habilidade disponível",
                   str "Use ↑/↓ para navegar, ENTER para ver descrição, ESC para fechar"
                 ]
  where
    asset = viewAsset viewState
    selectedSkillIdx = viewSkillSelected viewState
    skills = GameContext.assetSkills asset
    skillWidgets = zipWith (curry (renderSkillItem selectedSkillIdx)) [0 ..] skills

    renderSkillItem :: Int -> (Int, GameContext.AssetSkill) -> Widget Name
    renderSkillItem selectedIdx (idx, skill) =
      let isSelected = idx == selectedIdx
          skillNumber = GameContext.skillIndex skill
          skillNameText = GameContext.skillName skill

          skillPrefix = case viewPlayerAsset viewState of
            Nothing -> "- "
            Just playerAsset ->
              if skillNumber `elem` GameContext.enabledSkills playerAsset
                then "* "
                else "- "
          widget =
            if isSelected
              then
                withAttr choiceSelectedAttr $
                  vBox [str $ "> " <> skillPrefix <> T.unpack skillNameText]
              else vBox [str $ "  " <> skillPrefix <> T.unpack skillNameText]
       in vBox [widget, str " "]

assetTypeToText :: GameContext.AssetType -> T.Text
assetTypeToText GameContext.Companion = "Companheiro"
assetTypeToText GameContext.Path = "Caminho"
assetTypeToText GameContext.CombatTalent = "Talento de Combate"
assetTypeToText GameContext.Ritual = "Ritual"
assetTypeToText GameContext.Module = "Módulo"
assetTypeToText GameContext.SupportVehicle = "Veículo de Apoio"
assetTypeToText GameContext.CommandVehicle = "Veículo de Comando"
assetTypeToText GameContext.Deed = "Feito"
