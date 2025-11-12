{-# LANGUAGE DeriveGeneric #-}

module System.Network.Types
  ( ClientConnection (..),
    ServerState (..),
    ClientState (..),
    NetworkRole (..),
  )
where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TChan)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Socket (PortNumber, Socket)
import System.Tui.Comm (GameOutput)

data NetworkRole
  = HostRole
  | ClientRole
  | SoloRole
  deriving (Eq, Show, Generic)

data ClientConnection = ClientConnection
  { connSocket :: Socket,
    connPlayerName :: T.Text,
    connCharacterName :: T.Text,
    connConnected :: Bool,
    connReadBuffer :: MVar BL.ByteString
  }
  deriving (Eq)

data ServerState = ServerState
  { serverSocket :: Socket,
    serverClients :: MVar [ClientConnection],
    serverPort :: PortNumber,
    serverPlayerName :: T.Text,
    serverCharacterName :: T.Text,
    serverTuiChannel :: Maybe (TChan GameOutput),
    pendingConnections :: MVar [ClientConnection]
  }
  deriving (Eq)

data ClientState = ClientState
  { clientSocket :: Socket,
    clientHost :: String,
    clientPort :: PortNumber,
    clientConnected :: MVar Bool,
    clientPlayerName :: T.Text,
    clientCharacterName :: T.Text
  }
  deriving (Eq)
