module System.GameContract (Handle (..)) where

newtype Handle = Handle {runGame :: IO ()}