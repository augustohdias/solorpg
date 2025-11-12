{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module System.Network.Protocol
  ( NetworkMessage (..)
  , MessageType (..)
  , encodeMessage
  , decodeMessage
  , decodeMessageWithRest
  , validateMessageSize
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Data.Int (Int64)


data NetworkMessage
  = ConnectionRequest
      { reqPlayerName :: T.Text
      , reqCharacterName :: T.Text
      }
  | ConnectionAccepted
      { accSessionId :: T.Text
      }
  | ConnectionRejected
  | StoryLogEntry
      { logPlayerName :: T.Text
      , logText :: T.Text
      , logTimestamp :: UTCTime
      }
  | ProgressTrackSync
      { syncPlayerName :: T.Text
      , syncTrackId :: T.Text
      , syncTrackData :: T.Text
      }
  | SharedVowCreated
      { vowPlayerName :: T.Text
      , vowTrackId :: T.Text
      , vowTrackData :: T.Text
      }
  | SharedVowProgress
      { vowProgressPlayerName :: T.Text
      , vowProgressTrackId :: T.Text
      , vowProgressTicks :: Int
      }
  | SharedVowCompleted
      { vowCompletedPlayerName :: T.Text
      , vowCompletedTrackId :: T.Text
      , vowCompletedExperience :: Int
      }
  | Heartbeat
  deriving (Eq, Show, Generic, Aeson.ToJSON, Aeson.FromJSON)


data MessageType
  = TypeConnectionRequest
  | TypeConnectionAccepted
  | TypeConnectionRejected
  | TypeStoryLogEntry
  | TypeProgressTrackSync
  | TypeSharedVowCreated
  | TypeSharedVowProgress
  | TypeSharedVowCompleted
  | TypeHeartbeat
  deriving (Eq, Show)

maxMessageSize :: Int
maxMessageSize = 1000


validateMessageSize :: T.Text -> Bool
validateMessageSize text = T.length text <= maxMessageSize


encodeMessage :: NetworkMessage -> BL.ByteString
encodeMessage = Aeson.encode


decodeMessage :: BL.ByteString -> Maybe NetworkMessage
decodeMessage = Aeson.decode




decodeMessageWithRest :: BL.ByteString -> Maybe (NetworkMessage, BL.ByteString)
decodeMessageWithRest bytes = do
  msg <- Aeson.decode bytes

  let findJsonEnd i (depth :: Int) inString escaped
        | i >= BL.length bytes = BL.length bytes
        | otherwise =
            let byte = BLC.index bytes i
            in if escaped
               then findJsonEnd (i + 1) depth inString False
               else case byte of
                 '\\' -> findJsonEnd (i + 1) depth inString True
                 '"' -> findJsonEnd (i + 1) depth (not inString) False
                 '{' | not inString -> findJsonEnd (i + 1) (depth + 1) inString False
                 '}' | not inString && depth > 0 ->
                   if depth == 1
                   then i + 1
                   else findJsonEnd (i + 1) (depth - 1) inString False
                 '[' | not inString -> findJsonEnd (i + 1) (depth + 1) inString False
                 ']' | not inString && depth > 0 ->
                   if depth == 1
                   then i + 1
                   else findJsonEnd (i + 1) (depth - 1) inString False
                 _ -> findJsonEnd (i + 1) depth inString False
      jsonEnd = findJsonEnd (0 :: Int64) (0 :: Int) False False
      rest = BL.drop jsonEnd bytes
      restWithoutWhitespace = BLC.dropWhile (`elem` [' ', '\t', '\n', '\r']) rest

  Just (msg, restWithoutWhitespace)
