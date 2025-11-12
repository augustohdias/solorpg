{-# LANGUAGE OverloadedStrings #-}
{- | Safe IO utilities for concurrent file operations
     
     Provides file locking and safe concurrent file access to prevent
     race conditions and data corruption.
-}
module System.Util.SafeIO 
    ( safeWriteFile
    , safeReadFile
    ) where

import qualified Data.ByteString.Lazy as BL
import Control.Concurrent.MVar (MVar, newMVar, withMVar, takeMVar, putMVar)
import Control.Exception (catch, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map



{-# NOINLINE fileLocks #-}
fileLocks :: MVar (Map.Map FilePath (MVar ()))
fileLocks = unsafePerformIO (newMVar Map.empty)


getFileLock :: FilePath -> IO (MVar ())
getFileLock path = do
    locks <- takeMVar fileLocks
    case Map.lookup path locks of
        Just lock -> do
            putMVar fileLocks locks
            return lock
        Nothing -> do
            newLock <- newMVar ()
            putMVar fileLocks (Map.insert path newLock locks)
            return newLock



safeWriteFile :: FilePath -> BL.ByteString -> IO (Either SomeException ())
safeWriteFile path contents = do
    lock <- getFileLock path
    result <- withMVar lock $ \_ -> 
        catch (BL.writeFile path contents >> return (Right ()))
              (\e -> return (Left (e :: SomeException)))
    return result



safeReadFile :: FilePath -> IO (Either SomeException BL.ByteString)
safeReadFile path = do
    lock <- getFileLock path
    result <- withMVar lock $ \_ ->
        catch (BL.readFile path >>= \c -> return (Right c))
              (\e -> return (Left (e :: SomeException)))
    return result

