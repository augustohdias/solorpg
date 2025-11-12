{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Network.Client
  ( connectToServer
  , disconnectFromServer
  , sendMessage
  , startReceiveLoop
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, readMVar)
import Control.Exception (catch, IOException, SomeException, try)
import Control.Monad (forever, void, when)
import Data.Foldable (for_)
import qualified Data.ByteString.Lazy as BL
import Network.Socket
  ( Socket
  , SocketType (Stream)
  , connect
  , close
  , defaultProtocol
  , socket
  , AddrInfo (addrAddress, addrSocketType, addrFamily, addrProtocol)
  , defaultHints
  , getAddrInfo
  , Family (AF_INET)
  , PortNumber
  )
import qualified Network.Socket.ByteString.Lazy as NSB
import qualified System.Network.Protocol as Protocol
import qualified System.Network.Types as Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Aeson as Aeson
import Control.Concurrent.STM (TChan, atomically, writeTChan)
import System.Tui.Comm (GameOutput (..), MessageType (..))
import qualified System.GameContext as GameContext
import qualified System.Progress as Progress
import qualified System.Util.Parser as Parser

-- | Conecta a um servidor
connectToServer :: String -> PortNumber -> T.Text -> T.Text -> IO (Either String Types.ClientState)
connectToServer host port playerName characterName = do
  result <- try $ do
    addrInfos <- getAddrInfo (Just (defaultHints { addrFamily = AF_INET, addrSocketType = Stream })) (Just host) (Just (show port))
    
    case addrInfos of
      [] -> error "No address info found"
      (info:_) -> do
        sock <- socket (addrFamily info) (addrSocketType info) (addrProtocol info)
        connect sock (addrAddress info)
        
        connected <- newMVar True
        
        let clientState = Types.ClientState
              { Types.clientSocket = sock
              , Types.clientHost = host
              , Types.clientPort = port
              , Types.clientConnected = connected
              , Types.clientPlayerName = playerName
              , Types.clientCharacterName = characterName
              }
        
        -- Envia mensagem de conexão
        let connReq = Protocol.ConnectionRequest playerName characterName
        sendMessageToSocket sock connReq
        
        -- Aguarda resposta
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

-- | Desconecta do servidor
disconnectFromServer :: Types.ClientState -> IO ()
disconnectFromServer clientState = do
  modifyMVar_ (Types.clientConnected clientState) $ \_ -> return False
  close (Types.clientSocket clientState)

-- | Envia uma mensagem para o servidor
sendMessage :: Types.ClientState -> Protocol.NetworkMessage -> IO (Either String ())
sendMessage clientState msg = do
  connected <- readMVar (Types.clientConnected clientState)
  if connected
    then do
      result <- try $ sendMessageToSocket (Types.clientSocket clientState) msg
      case result of
        Left (e :: IOException) -> do
          -- Marca como desconectado em caso de erro
          modifyMVar_ (Types.clientConnected clientState) $ \_ -> return False
          return $ Left $ show e
        Right () -> return $ Right ()
    else return $ Left "Not connected"

-- | Envia mensagem para um socket
sendMessageToSocket :: Socket -> Protocol.NetworkMessage -> IO ()
sendMessageToSocket sock msg = do
  let encoded = Protocol.encodeMessage msg
  NSB.sendAll sock encoded

-- | Tenta ler uma mensagem de um socket
tryReadMessage :: Socket -> IO (Maybe Protocol.NetworkMessage)
tryReadMessage sock = do
  result <- try $ NSB.recv sock 4096
  case result of
    Left (_ :: IOException) -> return Nothing
    Right bytes
      | BL.null bytes -> return Nothing
      | otherwise -> return $ Protocol.decodeMessage bytes

-- | Inicia loop de recepção de mensagens do servidor
startReceiveLoop :: Types.ClientState -> TChan GameOutput -> IO ()
startReceiveLoop clientState outputChan = do
  -- Inicia loop de recepção
  void $ forkIO $ receiveLoop clientState outputChan
  -- Inicia loop de heartbeat
  void $ forkIO $ heartbeatLoop clientState

-- | Loop de heartbeat - envia heartbeat para o servidor a cada 30 segundos
heartbeatLoop :: Types.ClientState -> IO ()
heartbeatLoop clientState = loop
  where
    loop = do
      connected <- readMVar (Types.clientConnected clientState)
      if not connected
        then return ()  -- Sai do loop quando desconectado
        else do
          threadDelay (30 * 1000000) -- 30 segundos
          connectedAfter <- readMVar (Types.clientConnected clientState)
          when connectedAfter $ do
            result <- try $ sendMessageToSocket (Types.clientSocket clientState) Protocol.Heartbeat
            case result of
              Left (_ :: IOException) -> do
                -- Servidor desconectado
                modifyMVar_ (Types.clientConnected clientState) $ \_ -> return False
                close (Types.clientSocket clientState)
              Right () -> loop  -- Continua o loop

-- | Loop de recepção de mensagens
receiveLoop :: Types.ClientState -> TChan GameOutput -> IO ()
receiveLoop clientState outputChan = 
  (`catch` (\(e :: IOException) -> do
    modifyMVar_ (Types.clientConnected clientState) $ \_ -> return False
    atomically $ writeTChan outputChan (LogEntry "Conexão perdida com o servidor" SystemMessage)
    close (Types.clientSocket clientState)
    -- Atualiza estado de multiplayer no contexto
    maybeCtx <- GameContext.getCurrentContext
    for_ maybeCtx $ \ctx -> do
      let updatedCtx = ctx
            { GameContext.isMultiplayer = False
            , GameContext.multiplayerSessionId = Nothing
            }
      void $ GameContext.saveContext updatedCtx
    return ())) $
  loop
  where
    loop = do
      connected <- readMVar (Types.clientConnected clientState)
      if not connected
        then return ()  -- Sai do loop quando desconectado
        else do
          maybeMsg <- tryReadMessage (Types.clientSocket clientState)
          
          case maybeMsg of
            Nothing -> do
              -- Erro ao ler ou conexão fechada pelo servidor
              modifyMVar_ (Types.clientConnected clientState) $ \_ -> return False
              close (Types.clientSocket clientState)
              atomically $ writeTChan outputChan (LogEntry "Desconectado do servidor" SystemMessage)
              -- Atualiza estado de multiplayer no contexto
              maybeCtx <- GameContext.getCurrentContext
              for_ maybeCtx $ \ctx -> do
                let updatedCtx = ctx
                      { GameContext.isMultiplayer = False
                      , GameContext.multiplayerSessionId = Nothing
                      }
                void $ GameContext.saveContext updatedCtx
              return ()  -- Sai do loop
              
            Just Protocol.Heartbeat -> do
              -- Heartbeat recebido, continua o loop
              loop
              
            Just (Protocol.StoryLogEntry playerName logText _timestamp) -> do
              -- Story log recebido, adiciona ao contexto local
              maybeCtx <- GameContext.getCurrentContext
              case maybeCtx of
                Just ctx -> do
                  updatedCtx <- GameContext.addSyncLogEntry ctx logText
                  void $ GameContext.saveContext updatedCtx
                Nothing -> return ()
              atomically $ writeTChan outputChan (LogEntry logText NarrativeMessage)
              loop
              
            Just (Protocol.SharedVowCreated playerName trackId trackData) -> do
              -- Shared vow criado, adiciona ao contexto
              maybeCtx <- GameContext.getCurrentContext
              case maybeCtx of
                Just ctx -> do
                  -- Deserializa track do JSON
                  case Aeson.decodeStrict (TE.encodeUtf8 trackData) of
                    Nothing -> return ()
                    Just (track :: Progress.ProgressTrack) -> do
                      -- Adiciona novo shared vow
                      updatedCtx <- GameContext.addProgressTrack ctx track
                      void $ GameContext.saveContext updatedCtx
                      atomically $ writeTChan outputChan (LogEntry (T.concat ["Shared vow criado por ", playerName, ": ", trackId]) SystemMessage)
                Nothing -> return ()
              loop
              
            Just (Protocol.SharedVowProgress playerName trackId ticks) -> do
              -- Progresso em shared vow
              maybeCtx <- GameContext.getCurrentContext
              case maybeCtx of
                Just ctx -> do
                  case GameContext.getProgressTrack ctx trackId of
                    Nothing -> return ()
                    Just track -> do
                      -- Adiciona ticks ao track
                      updatedTrack <- Progress.markProgressTicks track ticks
                      updatedCtx <- GameContext.updateProgressTrack ctx trackId updatedTrack
                      void $ GameContext.saveContext updatedCtx
                      atomically $ writeTChan outputChan (LogEntry (T.concat ["Progresso em shared vow por ", playerName]) SystemMessage)
                Nothing -> return ()
              loop
              
            Just (Protocol.SharedVowCompleted playerName trackId experience) -> do
              -- Shared vow completado, adiciona experiência compartilhada
              maybeCtx <- GameContext.getCurrentContext
              case maybeCtx of
                Just ctx -> do
                  -- Adiciona experiência compartilhada ao contexto usando parseResourceAdd
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
              
            Just (Protocol.ProgressTrackSync playerName trackId trackData) -> do
              -- Progress track sync (combate/jornada)
              maybeCtx <- GameContext.getCurrentContext
              case maybeCtx of
                Just ctx -> do
                  -- Deserializa track do JSON
                  case Aeson.decodeStrict (TE.encodeUtf8 trackData) of
                    Nothing -> return ()
                    Just (track :: Progress.ProgressTrack) -> do
                      -- Verifica se track já existe
                      case GameContext.getProgressTrack ctx trackId of
                        Nothing -> do
                          -- Adiciona novo track
                          updatedCtx <- GameContext.addProgressTrack ctx track
                          void $ GameContext.saveContext updatedCtx
                        Just _ -> do
                          -- Atualiza track existente
                          updatedCtx <- GameContext.updateProgressTrack ctx trackId track
                          void $ GameContext.saveContext updatedCtx
                Nothing -> return ()
              loop
              
            Just _ -> do
              -- Outro tipo de mensagem, ignora e continua
              loop
