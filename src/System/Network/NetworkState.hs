module System.Network.NetworkState
  ( NetworkState (..),
    getNetworkState,
    setNetworkState,
    clearNetworkState,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Network.Types as Types

data NetworkState
  = ServerState Types.ServerState
  | ClientState Types.ClientState
  | NoNetworkState
  deriving (Eq)

networkStateRef :: MVar NetworkState
networkStateRef = unsafePerformIO (newMVar NoNetworkState)
{-# NOINLINE networkStateRef #-}

getNetworkState :: IO NetworkState
getNetworkState = readMVar networkStateRef

setNetworkState :: NetworkState -> IO ()
setNetworkState state = modifyMVar networkStateRef $ \_ -> return (state, ())

clearNetworkState :: IO ()
clearNetworkState = setNetworkState NoNetworkState
