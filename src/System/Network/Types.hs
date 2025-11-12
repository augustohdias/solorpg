{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Network.Types
  ( ClientConnection (..)
  , ServerState (..)
  , ClientState (..)
  , NetworkRole (..)
  ) where

import Network.Socket (Socket, PortNumber)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM (TChan)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import System.Tui.Comm (GameOutput)

-- | Papel do jogador na sessão multiplayer
data NetworkRole
  = HostRole      -- ^ Host da sessão
  | ClientRole    -- ^ Cliente conectado
  | SoloRole      -- ^ Modo solo (sem multiplayer)
  deriving (Eq, Show, Generic)

-- | Representa uma conexão de cliente no servidor
data ClientConnection = ClientConnection
  { connSocket :: Socket
  , connPlayerName :: T.Text
  , connCharacterName :: T.Text
  , connConnected :: Bool
  , connReadBuffer :: MVar BL.ByteString  -- Buffer para dados não processados
  } deriving (Eq)

-- | Estado do servidor
data ServerState = ServerState
  { serverSocket :: Socket
  , serverClients :: MVar [ClientConnection]
  , serverPort :: PortNumber
  , serverPlayerName :: T.Text
  , serverCharacterName :: T.Text
  , serverTuiChannel :: Maybe (TChan GameOutput)  -- Canal para enviar prompts à TUI
  , pendingConnections :: MVar [ClientConnection]  -- Conexões pendentes de aprovação
  } deriving (Eq)

-- | Estado do cliente
data ClientState = ClientState
  { clientSocket :: Socket
  , clientHost :: String
  , clientPort :: PortNumber
  , clientConnected :: MVar Bool
  , clientPlayerName :: T.Text
  , clientCharacterName :: T.Text
  } deriving (Eq)
