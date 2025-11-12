{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Network.Server
  ( startServer,
    stopServer,
    broadcastMessage,
    getLocalIP,
    defaultPort,
    approveConnection,
    rejectPendingConnection,
    startReceiveLoop,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad (forM_, forever, void)
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import qualified Data.Text as T
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    Family (AF_INET),
    PortNumber,
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Datagram, Stream),
    accept,
    bind,
    close,
    connect,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    hostAddressToTuple,
    listen,
    setSocketOption,
    socket,
    tupleToHostAddress,
  )
import qualified Network.Socket.ByteString.Lazy as NSB
import qualified System.GameContext as GameContext
import qualified System.Network.Protocol as Protocol
import qualified System.Network.Types as Types
import System.Tui.Comm (GameOutput (..), MessageType (..))

defaultPort :: PortNumber
defaultPort = 9876

startServer :: PortNumber -> T.Text -> T.Text -> Maybe (TChan GameOutput) -> IO (Either String Types.ServerState)
startServer port playerName characterName maybeTuiChan = do
  result <- try $ do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port (tupleToHostAddress (0, 0, 0, 0)))
    listen sock 5

    clients <- newMVar []
    pending <- newMVar []

    let serverState =
          Types.ServerState
            { Types.serverSocket = sock,
              Types.serverClients = clients,
              Types.serverPort = port,
              Types.serverPlayerName = playerName,
              Types.serverCharacterName = characterName,
              Types.serverTuiChannel = maybeTuiChan,
              Types.pendingConnections = pending
            }

    void $ forkIO $ acceptConnectionsLoop serverState

    void $ forkIO $ heartbeatLoop serverState

    return serverState

  case result of
    Left (e :: SomeException) -> return $ Left $ show e
    Right state -> return $ Right state

acceptConnectionsLoop :: Types.ServerState -> IO ()
acceptConnectionsLoop serverState =
  forever $
    do
      (clientSock, _) <- accept (Types.serverSocket serverState)

      void $ forkIO $ handleClientConnection serverState clientSock
      `catch` (\(_ :: IOException) -> return ())

handleClientConnection :: Types.ServerState -> Socket -> IO ()
handleClientConnection serverState clientSock =
  (`catch` (\(_ :: IOException) -> close clientSock)) $ do
    buffer <- newMVar BL.empty

    maybeMsg <- tryReadMessageWithBuffer clientSock buffer

    case maybeMsg of
      Just (Protocol.ConnectionRequest playerName charName) -> do
        let connection =
              Types.ClientConnection
                { Types.connSocket = clientSock,
                  Types.connPlayerName = playerName,
                  Types.connCharacterName = charName,
                  Types.connConnected = False,
                  Types.connReadBuffer = buffer
                }

        modifyMVar_ (Types.pendingConnections serverState) $ \pending ->
          return (connection : pending)

        case Types.serverTuiChannel serverState of
          Just tuiChan -> do
            atomically $ writeTChan tuiChan (ConnectionRequestPrompt playerName charName)
          Nothing -> do
            acceptConnection serverState connection

            startReceiveLoop serverState connection
      Just _ -> do
        close clientSock
      Nothing -> do
        close clientSock

acceptConnection :: Types.ServerState -> Types.ClientConnection -> IO ()
acceptConnection serverState connection = do
  let sessionId = "session-" ++ show (Types.serverPort serverState)
  let acceptMsg = Protocol.ConnectionAccepted {Protocol.accSessionId = T.pack sessionId}

  sendMessage (Types.connSocket connection) acceptMsg

  modifyMVar_ (Types.pendingConnections serverState) $ \pending ->
    return $ filter (\c -> Types.connSocket c /= Types.connSocket connection) pending

  modifyMVar_ (Types.serverClients serverState) $ \clients ->
    return $ (connection {Types.connConnected = True}) : clients

  clientsAfter <- readMVar (Types.serverClients serverState)
  let connectedPlayers = map Types.connPlayerName $ filter Types.connConnected clientsAfter
  case Types.serverTuiChannel serverState of
    Just tuiChan -> atomically $ writeTChan tuiChan (PlayerList connectedPlayers)
    Nothing -> return ()

rejectConnection :: Types.ServerState -> Types.ClientConnection -> IO ()
rejectConnection serverState connection = do
  sendMessage (Types.connSocket connection) Protocol.ConnectionRejected
  close (Types.connSocket connection)

  modifyMVar_ (Types.pendingConnections serverState) $ \pending ->
    return $ filter (\c -> Types.connSocket c /= Types.connSocket connection) pending

  modifyMVar_ (Types.serverClients serverState) $ \clients ->
    return $ filter (\c -> Types.connSocket c /= Types.connSocket connection) clients

approveConnection :: Types.ServerState -> T.Text -> IO (Maybe Types.ClientConnection)
approveConnection serverState playerName = do
  result <- modifyMVar (Types.pendingConnections serverState) $ \pending ->
    if T.null playerName
      then case pending of
        [] -> return (pending, Nothing)
        (conn : _) -> return (tail pending, Just conn)
      else case find (\c -> Types.connPlayerName c == playerName) pending of
        Nothing -> return (pending, Nothing)
        Just conn -> return (filter (\c -> Types.connPlayerName c /= playerName) pending, Just conn)

  case result of
    Nothing -> return Nothing
    Just conn -> do
      acceptConnection serverState conn

      startReceiveLoop serverState conn
      return (Just conn)

rejectPendingConnection :: Types.ServerState -> T.Text -> IO ()
rejectPendingConnection serverState playerName = do
  modifyMVar_ (Types.pendingConnections serverState) $ \pending ->
    if T.null playerName
      then case pending of
        [] -> return pending
        (conn : _) -> do
          rejectConnection serverState conn
          return $ tail pending
      else case find (\c -> Types.connPlayerName c == playerName) pending of
        Nothing -> return pending
        Just conn -> do
          rejectConnection serverState conn
          return $ filter (\c -> Types.connPlayerName c /= playerName) pending

heartbeatLoop :: Types.ServerState -> IO ()
heartbeatLoop serverState = forever $ do
  threadDelay (30 * 1000000)
  clients <- readMVar (Types.serverClients serverState)
  let connectedClients = filter Types.connConnected clients
  mapM_
    ( \conn -> do
        result <- try $ sendMessage (Types.connSocket conn) Protocol.Heartbeat
        case result of
          Left (_ :: IOException) -> do
            modifyMVar_ (Types.serverClients serverState) $ \clients' ->
              return $ filter (\c -> Types.connSocket c /= Types.connSocket conn) clients'
          Right () -> return ()
    )
    connectedClients

startReceiveLoop :: Types.ServerState -> Types.ClientConnection -> IO ()
startReceiveLoop serverState connection = void $ forkIO $ receiveLoop serverState connection

receiveLoop :: Types.ServerState -> Types.ClientConnection -> IO ()
receiveLoop serverState connection =
  ( `catch`
      ( \(_ :: IOException) -> do
          putStrLn "Erro de IO no loop de recepção"
          removeClient serverState connection
          return ()
      )
  )
    loop
  where
    loop = do
      maybeMsg <- tryReadMessageWithBuffer (Types.connSocket connection) (Types.connReadBuffer connection)

      case maybeMsg of
        Nothing -> do
          modifyMVar_ (Types.serverClients serverState) $ \clients ->
            return $ filter (\c -> Types.connSocket c /= Types.connSocket connection) clients
          close (Types.connSocket connection)
          clientsAfter <- readMVar (Types.serverClients serverState)
          let connectedPlayers = map Types.connPlayerName $ filter Types.connConnected clientsAfter
          case Types.serverTuiChannel serverState of
            Just tuiChan -> atomically $ writeTChan tuiChan (PlayerList connectedPlayers)
            Nothing -> return ()
          return ()
        Just Protocol.Heartbeat -> do
          loop
        Just (Protocol.StoryLogEntry playerName logText timestamp) -> do
          maybeCtx <- GameContext.getCurrentContext
          case maybeCtx of
            Just ctx -> do
              updatedCtx <- GameContext.addLogEntry ctx logText

              void $ forkIO $ do
                void $ GameContext.saveContext updatedCtx

              case Types.serverTuiChannel serverState of
                Just tuiChan -> atomically $ writeTChan tuiChan (LogEntry logText NarrativeMessage)
                Nothing -> return ()
            Nothing -> return ()

          broadcastMessage serverState (Protocol.StoryLogEntry playerName logText timestamp) (Just connection)
          loop
        Just (Protocol.SharedVowCreated playerName trackId trackData) -> do
          broadcastMessage serverState (Protocol.SharedVowCreated playerName trackId trackData) (Just connection)
          loop
        Just (Protocol.SharedVowProgress playerName trackId ticks) -> do
          broadcastMessage serverState (Protocol.SharedVowProgress playerName trackId ticks) (Just connection)
          loop
        Just (Protocol.SharedVowCompleted playerName trackId experience) -> do
          broadcastMessage serverState (Protocol.SharedVowCompleted playerName trackId experience) (Just connection)
          loop
        Just (Protocol.ProgressTrackSync playerName trackId trackData) -> do
          broadcastMessage serverState (Protocol.ProgressTrackSync playerName trackId trackData) (Just connection)
          loop
        Just _ -> do
          loop

removeClient :: Types.ServerState -> Types.ClientConnection -> IO ()
removeClient serverState connection = do
  close (Types.connSocket connection)
  modifyMVar_ (Types.serverClients serverState) $ \clients ->
    return $ filter (\c -> Types.connSocket c /= Types.connSocket connection) clients

  clientsAfter <- readMVar (Types.serverClients serverState)
  let connectedPlayers = map Types.connPlayerName $ filter Types.connConnected clientsAfter
  case Types.serverTuiChannel serverState of
    Just tuiChan -> atomically $ writeTChan tuiChan (PlayerList connectedPlayers)
    Nothing -> return ()

broadcastMessage :: Types.ServerState -> Protocol.NetworkMessage -> Maybe Types.ClientConnection -> IO ()
broadcastMessage serverState msg excludeConnection = do
  clients <- readMVar (Types.serverClients serverState)

  let targetClients = case excludeConnection of
        Just exclude -> filter (\c -> Types.connSocket c /= Types.connSocket exclude && Types.connConnected c) clients
        Nothing -> filter Types.connConnected clients

  forM_ targetClients $ \client -> do
    result <- try (sendMessage (Types.connSocket client) msg :: IO ())
    case result of
      Left (_ :: IOException) -> return ()
      Right () -> return ()

sendMessage :: Socket -> Protocol.NetworkMessage -> IO ()
sendMessage sock msg = do
  let encoded = Protocol.encodeMessage msg
  result <- try $ NSB.sendAll sock encoded
  case result of
    Left (_ :: IOException) -> return ()
    Right () -> return ()

tryReadMessageWithBuffer :: Socket -> MVar BL.ByteString -> IO (Maybe Protocol.NetworkMessage)
tryReadMessageWithBuffer sock buffer = do
  bufferedData <- readMVar buffer

  case Protocol.decodeMessageWithRest bufferedData of
    Just (msg, rest) -> do
      modifyMVar_ buffer $ \_ -> return rest
      return $ Just msg
    Nothing -> do
      result <- try $ NSB.recv sock 4096
      case result of
        Left (_ :: IOException) -> return Nothing
        Right bytes
          | BL.null bytes -> return Nothing
          | otherwise -> do
              modifyMVar_ buffer $ \old -> return (old `BL.append` bytes)

              newBuffered <- readMVar buffer
              case Protocol.decodeMessageWithRest newBuffered of
                Just (msg, rest) -> do
                  modifyMVar_ buffer $ \_ -> return rest
                  return $ Just msg
                Nothing -> return Nothing

_tryReadMessage :: Socket -> IO (Maybe Protocol.NetworkMessage)
_tryReadMessage sock = do
  result <- try $ NSB.recv sock 4096
  case result of
    Left (_ :: IOException) -> return Nothing
    Right bytes
      | BL.null bytes -> return Nothing
      | otherwise -> return $ Protocol.decodeMessage bytes

stopServer :: Types.ServerState -> IO ()
stopServer serverState = do
  clients <- readMVar (Types.serverClients serverState)
  mapM_ (close . Types.connSocket) clients
  close (Types.serverSocket serverState)

getLocalIP :: IO (Maybe String)
getLocalIP = getAllInterfaces

isLocalhost :: AddrInfo -> Bool
isLocalhost info = case addrAddress info of
  SockAddrInet _ addr ->
    let (a, _, _, _) = hostAddressToTuple addr
     in a == 127
  _ -> False

getAllInterfaces :: IO (Maybe String)
getAllInterfaces = do
  result <- try $ do
    targetInfos <- getAddrInfo (Just (defaultHints {addrFamily = AF_INET})) (Just "8.8.8.8") (Just "53")
    case targetInfos of
      [] -> getFirstNonLocalhostIP
      (targetInfo : _) -> do
        sock <- socket AF_INET Datagram defaultProtocol
        connect sock (addrAddress targetInfo)
        close sock

        getFirstNonLocalhostIP
  case result of
    Left (_ :: IOException) -> getFirstNonLocalhostIP
    Right ip -> return ip

getFirstNonLocalhostIP :: IO (Maybe String)
getFirstNonLocalhostIP = do
  result <- try $ do
    addrInfos <- getAddrInfo (Just (defaultHints {addrFamily = AF_INET})) (Just "0.0.0.0") Nothing
    let allInfos =
          if null addrInfos
            then []
            else addrInfos
    let validIPs =
          filter
            ( \info ->
                addrFamily info == AF_INET && not (isLocalhost info)
            )
            allInfos
    let privateIPs = filter isPrivateIP validIPs
    let selectedIPs = if null privateIPs then validIPs else privateIPs
    case selectedIPs of
      [] -> return Nothing
      (info : _) -> do
        let ipStr = case addrAddress info of
              SockAddrInet _ addr ->
                let (a, b, c, d) = hostAddressToTuple addr
                 in Just $ show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
              _ -> Nothing
        return ipStr
  case result of
    Left (_ :: IOException) -> return Nothing
    Right ip -> return ip

isPrivateIP :: AddrInfo -> Bool
isPrivateIP info = case addrAddress info of
  SockAddrInet _ addr ->
    let (a, b, _, _) = hostAddressToTuple addr
     in (a == 192 && b == 168)
          || (a == 10)
          || (a == 172 && b >= 16 && b <= 31)
  _ -> False
