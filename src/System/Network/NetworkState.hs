{-# LANGUAGE OverloadedStrings #-}

module System.Network.NetworkState
  ( NetworkState (..)
  , getNetworkState
  , setNetworkState
  , clearNetworkState
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Network.Types as Types
import qualified System.Network.Server as Server
import qualified System.Network.Client as Client
import qualified Data.Text as T

-- | Estado global de rede (servidor ou cliente)
data NetworkState
  = ServerState Types.ServerState
  | ClientState Types.ClientState
  | NoNetworkState
  deriving (Eq)

-- | Referência global para o estado de rede
networkStateRef :: MVar NetworkState
networkStateRef = unsafePerformIO (newMVar NoNetworkState)
{-# NOINLINE networkStateRef #-}

-- | Obtém o estado atual de rede
getNetworkState :: IO NetworkState
getNetworkState = readMVar networkStateRef

-- | Define o estado de rede
setNetworkState :: NetworkState -> IO ()
setNetworkState state = modifyMVar networkStateRef $ \_ -> return (state, ())

-- | Limpa o estado de rede
clearNetworkState :: IO ()
clearNetworkState = setNetworkState NoNetworkState
