{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Network.Server
  ( startServer
  , stopServer
  , broadcastMessage
  , getLocalIP
  , defaultPort
  , approveConnection
  , rejectPendingConnection
  , startReceiveLoop
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.Exception (catch, IOException, SomeException, try)
import Control.Monad (forever, void, when, forM_)
import qualified Data.ByteString.Lazy as BL
import Network.Socket
  ( Socket
  , SocketType (Stream, Datagram)
  , accept
  , bind
  , close
  , defaultProtocol
  , listen
  , setSocketOption
  , socket
  , SocketOption (ReuseAddr)
  , SockAddr (SockAddrInet, SockAddrInet6)
  , PortNumber
  , Family (AF_INET, AF_INET6)
  , AddrInfo (addrAddress, addrFamily)
  , defaultHints
  , getAddrInfo
  , tupleToHostAddress
  , hostAddressToTuple
  , connect
  )
import qualified Network.Socket.ByteString.Lazy as NSB
import qualified System.Network.Protocol as Protocol
import qualified System.Network.Types as Types
import qualified Data.Text as T
import Control.Exception (catch, IOException, SomeException, try)
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import System.Tui.Comm (GameOutput (..), MessageType (..))
import qualified System.GameContext as GameContext
import Data.List (find)

-- | Porta padrão do servidor
defaultPort :: PortNumber
defaultPort = 9876

-- | Inicia o servidor TCP
startServer :: PortNumber -> T.Text -> T.Text -> Maybe (TChan GameOutput) -> IO (Either String Types.ServerState)
startServer port playerName characterName maybeTuiChan = do
  result <- try $ do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port (tupleToHostAddress (0, 0, 0, 0)))
    listen sock 5
    
    clients <- newMVar []
    pending <- newMVar []
    
    let serverState = Types.ServerState
          { Types.serverSocket = sock
          , Types.serverClients = clients
          , Types.serverPort = port
          , Types.serverPlayerName = playerName
          , Types.serverCharacterName = characterName
          , Types.serverTuiChannel = maybeTuiChan
          , Types.pendingConnections = pending
          }
    
    -- Inicia thread para aceitar conexões
    void $ forkIO $ acceptConnectionsLoop serverState
    
    -- Inicia thread para heartbeat
    void $ forkIO $ heartbeatLoop serverState
    
    return serverState
  
  case result of
    Left (e :: SomeException) -> return $ Left $ show e
    Right state -> return $ Right state

-- | Loop para aceitar conexões de clientes
acceptConnectionsLoop :: Types.ServerState -> IO ()
acceptConnectionsLoop serverState = forever $ do
  (clientSock, _) <- accept (Types.serverSocket serverState)
  
  -- Inicia thread para lidar com este cliente
  void $ forkIO $ handleClientConnection serverState clientSock
  
  `catch` (\(e :: IOException) -> return ())

-- | Lida com uma conexão de cliente
handleClientConnection :: Types.ServerState -> Socket -> IO ()
handleClientConnection serverState clientSock = 
  (`catch` (\(e :: IOException) -> close clientSock)) $ do
    -- Cria buffer ANTES de ler qualquer dado
    buffer <- newMVar BL.empty
    
    -- Lê mensagem de conexão usando buffer para preservar dados extras
    maybeMsg <- tryReadMessageWithBuffer clientSock buffer
    
    case maybeMsg of
      Just (Protocol.ConnectionRequest playerName charName) -> do
        -- Cria conexão pendente com o buffer já populado
        let connection = Types.ClientConnection
              { Types.connSocket = clientSock
              , Types.connPlayerName = playerName
              , Types.connCharacterName = charName
              , Types.connConnected = False
              , Types.connReadBuffer = buffer
              }
        
        -- Adiciona à lista de conexões pendentes
        modifyMVar_ (Types.pendingConnections serverState) $ \pending ->
          return (connection : pending)
        
        -- Envia prompt para aprovação via TUI
        case Types.serverTuiChannel serverState of
          Just tuiChan -> do
            atomically $ writeTChan tuiChan (ConnectionRequestPrompt playerName charName)
            -- Loop de recepção será iniciado após aprovação em approveConnection
          Nothing -> do
            -- Se não há canal TUI, aceita automaticamente
            acceptConnection serverState connection
            -- Inicia loop de recepção para este cliente
            startReceiveLoop serverState connection
        
      Just _ -> do
        -- Mensagem inválida, fecha conexão
        close clientSock
      
      Nothing -> do
        -- Erro ao ler mensagem, fecha conexão
        close clientSock

-- | Aceita uma conexão de cliente
acceptConnection :: Types.ServerState -> Types.ClientConnection -> IO ()
acceptConnection serverState connection = do
  let sessionId = "session-" ++ show (Types.serverPort serverState)
  let acceptMsg = Protocol.ConnectionAccepted { Protocol.accSessionId = T.pack sessionId }
  
  sendMessage (Types.connSocket connection) acceptMsg
  
  -- Remove das pendentes e adiciona às conectadas
  modifyMVar_ (Types.pendingConnections serverState) $ \pending ->
    return $ filter (\c -> Types.connSocket c /= Types.connSocket connection) pending
  
  -- Marca como conectado e adiciona à lista de clientes
  -- Preserva o buffer existente
  modifyMVar_ (Types.serverClients serverState) $ \clients ->
    return $ (connection { Types.connConnected = True }) : clients
  
  -- Atualiza lista de jogadores na TUI (chamada após definir updatePlayerList)
  clientsAfter <- readMVar (Types.serverClients serverState)
  let connectedPlayers = map Types.connPlayerName $ filter Types.connConnected clientsAfter
  case Types.serverTuiChannel serverState of
    Just tuiChan -> atomically $ writeTChan tuiChan (PlayerList connectedPlayers)
    Nothing -> return ()

-- | Rejeita uma conexão de cliente
rejectConnection :: Types.ServerState -> Types.ClientConnection -> IO ()
rejectConnection serverState connection = do
  sendMessage (Types.connSocket connection) Protocol.ConnectionRejected
  close (Types.connSocket connection)
  
  -- Remove das pendentes
  modifyMVar_ (Types.pendingConnections serverState) $ \pending ->
    return $ filter (\c -> Types.connSocket c /= Types.connSocket connection) pending
  
  -- Remove da lista de clientes também (caso esteja lá)
  modifyMVar_ (Types.serverClients serverState) $ \clients ->
    return $ filter (\c -> Types.connSocket c /= Types.connSocket connection) clients

-- | Aprova uma conexão pendente (chamado quando host aceita)
-- Se playerName estiver vazio, aprova a primeira conexão pendente
approveConnection :: Types.ServerState -> T.Text -> IO (Maybe Types.ClientConnection)
approveConnection serverState playerName = do
  result <- modifyMVar (Types.pendingConnections serverState) $ \pending ->
    if T.null playerName
      then
        -- Aprova primeira conexão pendente
        case pending of
          [] -> return (pending, Nothing)
          (conn:_) -> return (tail pending, Just conn)
      else
        -- Aprova conexão específica por nome
        case find (\c -> Types.connPlayerName c == playerName) pending of
          Nothing -> return (pending, Nothing)
          Just conn -> return (filter (\c -> Types.connPlayerName c /= playerName) pending, Just conn)
  
  -- Após remover das pendentes, aceita a conexão e inicia receiveLoop
  case result of
    Nothing -> return Nothing
    Just conn -> do
      acceptConnection serverState conn
      -- Inicia receiveLoop em thread separada imediatamente após aceitar
      -- O socket já está pronto porque ConnectionAccepted foi enviado
      startReceiveLoop serverState conn
      return (Just conn)

-- | Rejeita uma conexão pendente (chamado quando host recusa)
-- Se playerName estiver vazio, rejeita a primeira conexão pendente
rejectPendingConnection :: Types.ServerState -> T.Text -> IO ()
rejectPendingConnection serverState playerName = do
  modifyMVar_ (Types.pendingConnections serverState) $ \pending ->
    if T.null playerName
      then
        -- Rejeita primeira conexão pendente
        case pending of
          [] -> return pending
          (conn:_) -> do
            rejectConnection serverState conn
            return $ tail pending
      else
        -- Rejeita conexão específica por nome
        case find (\c -> Types.connPlayerName c == playerName) pending of
          Nothing -> return pending
          Just conn -> do
            rejectConnection serverState conn
            return $ filter (\c -> Types.connPlayerName c /= playerName) pending

-- | Loop de heartbeat - envia heartbeat para todos os clientes a cada 30 segundos
heartbeatLoop :: Types.ServerState -> IO ()
heartbeatLoop serverState = forever $ do
  threadDelay (30 * 1000000) -- 30 segundos
  clients <- readMVar (Types.serverClients serverState)
  let connectedClients = filter Types.connConnected clients
  mapM_ (\conn -> do
    result <- try $ sendMessage (Types.connSocket conn) Protocol.Heartbeat
    case result of
      Left (_ :: IOException) -> do
        -- Cliente desconectado, remove da lista
        modifyMVar_ (Types.serverClients serverState) $ \clients' ->
          return $ filter (\c -> Types.connSocket c /= Types.connSocket conn) clients'
      Right () -> return ()
    ) connectedClients

-- | Inicia loop de recepção para um cliente conectado
startReceiveLoop :: Types.ServerState -> Types.ClientConnection -> IO ()
startReceiveLoop serverState connection = void $ forkIO $ receiveLoop serverState connection

-- | Loop de recepção de mensagens de um cliente
receiveLoop :: Types.ServerState -> Types.ClientConnection -> IO ()
receiveLoop serverState connection = 
  (`catch` (\(e :: IOException) -> do
    removeClient serverState connection
    return ())) $
  loop
  where
    loop = do
      -- CORRIGIDO: Usa buffer para ler mensagens, não recv direto
      maybeMsg <- tryReadMessageWithBuffer (Types.connSocket connection) (Types.connReadBuffer connection)
      
      case maybeMsg of
        Nothing -> do
          -- Erro ao ler mensagem ou conexão fechada (cliente desconectado)
          modifyMVar_ (Types.serverClients serverState) $ \clients ->
            return $ filter (\c -> Types.connSocket c /= Types.connSocket connection) clients
          close (Types.connSocket connection)
          -- Atualiza lista de jogadores
          clientsAfter <- readMVar (Types.serverClients serverState)
          let connectedPlayers = map Types.connPlayerName $ filter Types.connConnected clientsAfter
          case Types.serverTuiChannel serverState of
            Just tuiChan -> atomically $ writeTChan tuiChan (PlayerList connectedPlayers)
            Nothing -> return ()
          return ()  -- Sai do loop
        
        Just Protocol.Heartbeat -> do
          -- Heartbeat recebido, continua o loop
          loop
          
        Just (Protocol.StoryLogEntry playerName logText timestamp) -> do
          -- Story log recebido, adiciona ao contexto do host e faz broadcast para outros clientes
          -- Adiciona ao contexto do host (não bloqueia - operação rápida)
          maybeCtx <- GameContext.getCurrentContext
          case maybeCtx of
            Just ctx -> do
              updatedCtx <- GameContext.addLogEntry ctx logText
              -- Salva em thread separada para não bloquear
              void $ forkIO $ do
                void $ GameContext.saveContext updatedCtx
              -- Envia para a TUI do host imediatamente
              case Types.serverTuiChannel serverState of
                Just tuiChan -> atomically $ writeTChan tuiChan (LogEntry logText NarrativeMessage)
                Nothing -> return ()
            Nothing -> return ()
          -- Faz broadcast para outros clientes (não bloqueia - operação rápida)
          broadcastMessage serverState (Protocol.StoryLogEntry playerName logText timestamp) (Just connection)
          loop
          
        Just (Protocol.SharedVowCreated playerName trackId trackData) -> do
          -- Shared vow criado, faz broadcast
          broadcastMessage serverState (Protocol.SharedVowCreated playerName trackId trackData) (Just connection)
          loop
          
        Just (Protocol.SharedVowProgress playerName trackId ticks) -> do
          -- Progresso em shared vow, faz broadcast
          broadcastMessage serverState (Protocol.SharedVowProgress playerName trackId ticks) (Just connection)
          loop
          
        Just (Protocol.SharedVowCompleted playerName trackId experience) -> do
          -- Shared vow completado, faz broadcast para outros clientes
          broadcastMessage serverState (Protocol.SharedVowCompleted playerName trackId experience) (Just connection)
          loop
          
        Just (Protocol.ProgressTrackSync playerName trackId trackData) -> do
          -- Progress track sync (combate/jornada), faz broadcast
          broadcastMessage serverState (Protocol.ProgressTrackSync playerName trackId trackData) (Just connection)
          loop
          
        Just _ -> do
          -- Outro tipo de mensagem, ignora e continua
          loop

-- | Remove um cliente da lista
removeClient :: Types.ServerState -> Types.ClientConnection -> IO ()
removeClient serverState connection = do
  close (Types.connSocket connection)
  modifyMVar_ (Types.serverClients serverState) $ \clients ->
    return $ filter (\c -> Types.connSocket c /= Types.connSocket connection) clients
  -- Atualiza lista de jogadores na TUI
  clientsAfter <- readMVar (Types.serverClients serverState)
  let connectedPlayers = map Types.connPlayerName $ filter Types.connConnected clientsAfter
  case Types.serverTuiChannel serverState of
    Just tuiChan -> atomically $ writeTChan tuiChan (PlayerList connectedPlayers)
    Nothing -> return ()

-- | Faz broadcast de uma mensagem para todos os clientes (exceto o remetente)
broadcastMessage :: Types.ServerState -> Protocol.NetworkMessage -> Maybe Types.ClientConnection -> IO ()
broadcastMessage serverState msg excludeConnection = do
  clients <- readMVar (Types.serverClients serverState)
  
  let targetClients = case excludeConnection of
        Just exclude -> filter (\c -> Types.connSocket c /= Types.connSocket exclude && Types.connConnected c) clients
        Nothing -> filter Types.connConnected clients
  
  -- Envia mensagem para cada cliente diretamente (sem forkIO para garantir ordem)
  -- Ignora erros (cliente pode ter desconectado)
  forM_ targetClients $ \client -> do
    result <- try (sendMessage (Types.connSocket client) msg :: IO ())
    case result of
      Left (_ :: IOException) -> return ()  -- Cliente desconectado, ignora
      Right () -> return ()

-- | Envia uma mensagem para um socket
sendMessage :: Socket -> Protocol.NetworkMessage -> IO ()
sendMessage sock msg = do
  let encoded = Protocol.encodeMessage msg
  result <- try $ NSB.sendAll sock encoded
  case result of
    Left (_ :: IOException) -> return ()  -- Cliente desconectado, ignora erro
    Right () -> return ()

-- | Tenta ler uma mensagem de um socket com buffer
tryReadMessageWithBuffer :: Socket -> MVar BL.ByteString -> IO (Maybe Protocol.NetworkMessage)
tryReadMessageWithBuffer sock buffer = do
  -- Lê dados do buffer primeiro
  bufferedData <- readMVar buffer
  
  -- Tenta decodificar mensagem do buffer
  case Protocol.decodeMessageWithRest bufferedData of
    Just (msg, rest) -> do
      -- Mensagem decodificada com sucesso, mantém o resto no buffer
      modifyMVar_ buffer $ \_ -> return rest
      return $ Just msg
    Nothing -> do
      -- Não conseguiu decodificar do buffer, tenta ler mais dados do socket
      result <- try $ NSB.recv sock 4096
      case result of
        Left (_ :: IOException) -> return Nothing
        Right bytes
          | BL.null bytes -> return Nothing
          | otherwise -> do
              -- Adiciona novos dados ao buffer
              modifyMVar_ buffer $ \old -> return (old `BL.append` bytes)
              -- Tenta decodificar novamente
              newBuffered <- readMVar buffer
              case Protocol.decodeMessageWithRest newBuffered of
                Just (msg, rest) -> do
                  -- Mensagem decodificada, mantém o resto no buffer
                  modifyMVar_ buffer $ \_ -> return rest
                  return $ Just msg
                Nothing -> return Nothing

-- | Tenta ler uma mensagem de um socket (versão sem buffer para compatibilidade)
tryReadMessage :: Socket -> IO (Maybe Protocol.NetworkMessage)
tryReadMessage sock = do
  result <- try $ NSB.recv sock 4096
  case result of
    Left (_ :: IOException) -> return Nothing
    Right bytes
      | BL.null bytes -> return Nothing
      | otherwise -> return $ Protocol.decodeMessage bytes

-- | Para o servidor
stopServer :: Types.ServerState -> IO ()
stopServer serverState = do
  clients <- readMVar (Types.serverClients serverState)
  mapM_ (\c -> close (Types.connSocket c)) clients
  close (Types.serverSocket serverState)

-- | Obtém o IP local da máquina (IP real da interface de rede, não localhost)
getLocalIP :: IO (Maybe String)
getLocalIP = getAllInterfaces

-- | Verifica se um endereço é localhost
isLocalhost :: AddrInfo -> Bool
isLocalhost info = case addrAddress info of
  SockAddrInet _ addr -> 
    let (a, _, _, _) = hostAddressToTuple addr
    in a == 127
  _ -> False

-- | Tenta obter IP de todas as interfaces de rede
getAllInterfaces :: IO (Maybe String)
getAllInterfaces = do
  result <- try $ do
    -- Cria um socket temporário conectado a um endereço externo
    -- para descobrir qual interface de rede está sendo usada
    targetInfos <- getAddrInfo (Just (defaultHints { addrFamily = AF_INET })) (Just "8.8.8.8") (Just "53")
    case targetInfos of
      [] -> getFirstNonLocalhostIP
      (targetInfo:_) -> do
        -- Cria socket UDP e conecta (não envia dados, apenas configura rota)
        sock <- socket AF_INET Datagram defaultProtocol
        connect sock (addrAddress targetInfo)
        close sock
        -- Agora tenta obter todas as interfaces e pegar a primeira não-localhost
        getFirstNonLocalhostIP
  case result of
    Left (_ :: IOException) -> getFirstNonLocalhostIP
    Right ip -> return ip

-- | Obtém o primeiro IP não-localhost de todas as interfaces
getFirstNonLocalhostIP :: IO (Maybe String)
getFirstNonLocalhostIP = do
  result <- try $ do
    -- Obtém informações de todas as interfaces disponíveis
    -- Usando "0.0.0.0" como hostname para obter todas as interfaces locais
    addrInfos <- getAddrInfo (Just (defaultHints { addrFamily = AF_INET })) (Just "0.0.0.0") Nothing
    -- Se não funcionar, tenta sem especificar hostname
    let allInfos = if null addrInfos
          then []
          else addrInfos
    
    -- Filtra apenas IPv4 e não localhost, priorizando IPs privados (192.168.x.x, 10.x.x.x, 172.16-31.x.x)
    let validIPs = filter (\info -> 
          addrFamily info == AF_INET && not (isLocalhost info)) allInfos
    
    -- Prioriza IPs privados
    let privateIPs = filter isPrivateIP validIPs
    let selectedIPs = if null privateIPs then validIPs else privateIPs
    
    case selectedIPs of
      [] -> return Nothing
      (info:_) -> do
        let ipStr = case addrAddress info of
              SockAddrInet _ addr -> 
                let (a, b, c, d) = hostAddressToTuple addr
                in Just $ show a ++ "." ++ show b ++ "." ++ show c ++ "." ++ show d
              _ -> Nothing
        return ipStr
  case result of
    Left (_ :: IOException) -> return Nothing
    Right ip -> return ip

-- | Verifica se um IP é privado (192.168.x.x, 10.x.x.x, 172.16-31.x.x)
isPrivateIP :: AddrInfo -> Bool
isPrivateIP info = case addrAddress info of
  SockAddrInet _ addr -> 
    let (a, b, c, d) = hostAddressToTuple addr
    in (a == 192 && b == 168) ||  -- 192.168.x.x
       (a == 10) ||                -- 10.x.x.x
       (a == 172 && b >= 16 && b <= 31)  -- 172.16-31.x.x
  _ -> False
