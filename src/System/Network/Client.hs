{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Network.Client
  ( connectToServer,
    disconnectFromServer,
    sendMessage,
    startReceiveLoop,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (void, when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrProtocol, addrSocketType),
    Family (AF_INET),
    PortNumber,
    Socket,
    SocketType (Stream),
    close,
    connect,
    defaultHints,
    getAddrInfo,
    socket,
  )
import qualified Network.Socket.ByteString.Lazy as NSB
import qualified System.GameContext as GameContext
import qualified System.Network.Protocol as Protocol
import qualified System.Network.Types as Types
import qualified System.Progress as Progress
import System.Tui.Comm (GameOutput (..), MessageType (..))
import qualified System.Util.Parser as Parser

connectToServer :: String -> PortNumber -> T.Text -> T.Text -> IO (Either String Types.ClientState)
connectToServer host port playerName characterName = do
  result <- try $ do
    addrInfos <- getAddrInfo (Just (defaultHints {addrFamily = AF_INET, addrSocketType = Stream})) (Just host) (Just (show port))

    case addrInfos of
      [] -> error "No address info found"
      (info : _) -> do
        sock <- socket (addrFamily info) (addrSocketType info) (addrProtocol info)
        connect sock (addrAddress info)

        connected <- newMVar True

        let clientState =
              Types.ClientState
                { Types.clientSocket = sock,
                  Types.clientHost = host,
                  Types.clientPort = port,
                  Types.clientConnected = connected,
                  Types.clientPlayerName = playerName,
                  Types.clientCharacterName = characterName
                }

        let connReq = Protocol.ConnectionRequest playerName characterName
        sendMessageToSocket sock connReq

        maybeResponse <- tryReadMessage sock

        case maybeResponse of
          Just (Protocol.ConnectionAccepted {}) -> return $ Right clientState
          Just Protocol.ConnectionRejected -> do
            close sock
            return $ Left "Connection rejected by host"
          _ -> do
            close sock
            return $ Left "Invalid response from server"

  case result of
    Left (e :: SomeException) -> return $ Left $ show e
    Right result' -> return result'

disconnectFromServer :: Types.ClientState -> IO ()
disconnectFromServer clientState = do
  modifyMVar_ (Types.clientConnected clientState) $ \_ -> return False
  close (Types.clientSocket clientState)

sendMessage :: Types.ClientState -> Protocol.NetworkMessage -> IO (Either String ())
sendMessage clientState msg = do
  connected <- readMVar (Types.clientConnected clientState)
  if connected
    then do
      result <- try $ sendMessageToSocket (Types.clientSocket clientState) msg
      case result of
        Left (e :: IOException) -> do
          modifyMVar_ (Types.clientConnected clientState) $ \_ -> return False
          return $ Left $ show e
        Right () -> return $ Right ()
    else return $ Left "Not connected"

sendMessageToSocket :: Socket -> Protocol.NetworkMessage -> IO ()
sendMessageToSocket sock msg = do
  let encoded = Protocol.encodeMessage msg
  NSB.sendAll sock encoded

tryReadMessage :: Socket -> IO (Maybe Protocol.NetworkMessage)
tryReadMessage sock = do
  result <- try $ NSB.recv sock 4096
  case result of
    Left (_ :: IOException) -> return Nothing
    Right bytes
      | BL.null bytes -> return Nothing
      | otherwise -> return $ Protocol.decodeMessage bytes

startReceiveLoop :: Types.ClientState -> TChan GameOutput -> IO ()
startReceiveLoop clientState outputChan = do
  void $ forkIO $ receiveLoop clientState outputChan

  void $ forkIO $ heartbeatLoop clientState

heartbeatLoop :: Types.ClientState -> IO ()
heartbeatLoop clientState = loop
  where
    loop = do
      connected <- readMVar (Types.clientConnected clientState)
      if not connected
        then return ()
        else do
          threadDelay (30 * 1000000)
          connectedAfter <- readMVar (Types.clientConnected clientState)
          when connectedAfter $ do
            result <- try $ sendMessageToSocket (Types.clientSocket clientState) Protocol.Heartbeat
            case result of
              Left (_ :: IOException) -> do
                modifyMVar_ (Types.clientConnected clientState) $ \_ -> return False
                close (Types.clientSocket clientState)
              Right () -> loop

receiveLoop :: Types.ClientState -> TChan GameOutput -> IO ()
receiveLoop clientState outputChan =
  ( `catch`
      ( \(_ :: IOException) -> do
          modifyMVar_ (Types.clientConnected clientState) $ \_ -> return False
          atomically $ writeTChan outputChan (LogEntry "Conexão perdida com o servidor" SystemMessage)
          close (Types.clientSocket clientState)

          maybeCtx <- GameContext.getCurrentContext
          for_ maybeCtx $ \ctx -> do
            let updatedCtx =
                  ctx
                    { GameContext.isMultiplayer = False,
                      GameContext.multiplayerSessionId = Nothing
                    }
            void $ GameContext.saveContext updatedCtx
      )
  )
    loop
  where
    loop = do
      connected <- readMVar (Types.clientConnected clientState)
      if not connected
        then return ()
        else do
          maybeMsg <- tryReadMessage (Types.clientSocket clientState)

          case maybeMsg of
            Nothing -> do
              modifyMVar_ (Types.clientConnected clientState) $ \_ -> return False
              close (Types.clientSocket clientState)
              atomically $ writeTChan outputChan (LogEntry "Desconectado do servidor" SystemMessage)

              maybeCtx <- GameContext.getCurrentContext
              for_ maybeCtx $ \ctx -> do
                let updatedCtx =
                      ctx
                        { GameContext.isMultiplayer = False,
                          GameContext.multiplayerSessionId = Nothing
                        }
                void $ GameContext.saveContext updatedCtx
            Just Protocol.Heartbeat -> do
              loop
            Just (Protocol.StoryLogEntry _ logText _timestamp) -> do
              maybeCtx <- GameContext.getCurrentContext
              case maybeCtx of
                Just ctx -> do
                  updatedCtx <- GameContext.addSyncLogEntry ctx logText
                  void $ GameContext.saveContext updatedCtx
                Nothing -> return ()
              atomically $ writeTChan outputChan (LogEntry logText NarrativeMessage)
              loop
            Just (Protocol.SharedVowCreated playerName trackId trackData) -> do
              maybeCtx <- GameContext.getCurrentContext
              case maybeCtx of
                Just ctx -> do
                  case Aeson.decodeStrict (TE.encodeUtf8 trackData) of
                    Nothing -> return ()
                    Just (track :: Progress.ProgressTrack) -> do
                      updatedCtx <- GameContext.addProgressTrack ctx track
                      void $ GameContext.saveContext updatedCtx
                      atomically $ writeTChan outputChan (LogEntry (T.concat ["Shared vow criado por ", playerName, ": ", trackId]) SystemMessage)
                Nothing -> return ()
              loop
            Just (Protocol.SharedVowProgress playerName trackId ticks) -> do
              maybeCtx <- GameContext.getCurrentContext
              case maybeCtx of
                Just ctx -> do
                  case GameContext.getProgressTrack ctx trackId of
                    Nothing -> return ()
                    Just track -> do
                      updatedTrack <- Progress.markProgressTicks track ticks
                      updatedCtx <- GameContext.updateProgressTrack ctx trackId updatedTrack
                      void $ GameContext.saveContext updatedCtx
                      atomically $ writeTChan outputChan (LogEntry (T.concat ["Progresso em shared vow por ", playerName]) SystemMessage)
                Nothing -> return ()
              loop
            Just (Protocol.SharedVowCompleted playerName _ experience) -> do
              maybeCtx <- GameContext.getCurrentContext
              case maybeCtx of
                Just ctx -> do
                  let oldRes = GameContext.resources (GameContext.mainCharacter ctx)
                  let command = "experience:+" <> T.pack (show experience)
                  case Parser.parseResourceAdd command oldRes of
                    Nothing -> return ()
                    Just newRes -> do
                      updatedCtx <- GameContext.updateResources ctx newRes
                      void $ GameContext.saveContext updatedCtx
                      atomically $ writeTChan outputChan (LogEntry (T.concat ["Shared vow completado por ", playerName, "! Experiência compartilhada: +", T.pack (show experience)]) SystemMessage)
                Nothing -> return ()
              loop
            Just (Protocol.ProgressTrackSync _ trackId trackData) -> do
              maybeCtx <- GameContext.getCurrentContext
              case maybeCtx of
                Just ctx -> do
                  case Aeson.decodeStrict (TE.encodeUtf8 trackData) of
                    Nothing -> return ()
                    Just (track :: Progress.ProgressTrack) -> do
                      case GameContext.getProgressTrack ctx trackId of
                        Nothing -> do
                          updatedCtx <- GameContext.addProgressTrack ctx track
                          void $ GameContext.saveContext updatedCtx
                        Just _ -> do
                          updatedCtx <- GameContext.updateProgressTrack ctx trackId track
                          void $ GameContext.saveContext updatedCtx
                Nothing -> return ()
              loop
            Just _ -> do
              loop
