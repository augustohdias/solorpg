{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- | Tipos de mensagens de rede
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
      , syncTrackData :: T.Text  -- JSON serializado do ProgressTrack
      }
  | SharedVowCreated
      { vowPlayerName :: T.Text
      , vowTrackId :: T.Text
      , vowTrackData :: T.Text  -- JSON serializado do ProgressTrack
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

-- | Tipo de mensagem (para facilitar pattern matching)
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

-- | Obtém o tipo de uma mensagem
getMessageType :: NetworkMessage -> MessageType
getMessageType msg = case msg of
  ConnectionRequest {} -> TypeConnectionRequest
  ConnectionAccepted {} -> TypeConnectionAccepted
  ConnectionRejected -> TypeConnectionRejected
  StoryLogEntry {} -> TypeStoryLogEntry
  ProgressTrackSync {} -> TypeProgressTrackSync
  SharedVowCreated {} -> TypeSharedVowCreated
  SharedVowProgress {} -> TypeSharedVowProgress
  SharedVowCompleted {} -> TypeSharedVowCompleted
  Heartbeat -> TypeHeartbeat

-- | Limite máximo de caracteres por mensagem
maxMessageSize :: Int
maxMessageSize = 1000

-- | Valida o tamanho de uma mensagem de texto
validateMessageSize :: T.Text -> Bool
validateMessageSize text = T.length text <= maxMessageSize

-- | Codifica uma mensagem para envio pela rede
encodeMessage :: NetworkMessage -> BL.ByteString
encodeMessage = Aeson.encode

-- | Decodifica uma mensagem recebida da rede
decodeMessage :: BL.ByteString -> Maybe NetworkMessage
decodeMessage = Aeson.decode

-- | Decodifica uma mensagem e retorna o resto dos dados não processados
-- Usa uma abordagem simples: tenta encontrar onde termina o primeiro objeto JSON
-- procurando pelo último '}' ou ']' seguido de whitespace ou fim de dados
decodeMessageWithRest :: BL.ByteString -> Maybe (NetworkMessage, BL.ByteString)
decodeMessageWithRest bytes = do
  msg <- Aeson.decode bytes
  -- Encontra onde termina o primeiro objeto JSON válido
  -- Procura pelo último '}' ou ']' que fecha um objeto/array JSON válido
  let findJsonEnd i depth inString escaped
        | i >= BL.length bytes = BL.length bytes  -- Fim dos dados
        | otherwise =
            let byte = BLC.index bytes i
            in if escaped
               then findJsonEnd (i + 1) depth inString False  -- Próximo caractere após escape
               else case byte of
                 '\\' -> findJsonEnd (i + 1) depth inString True
                 '"' -> findJsonEnd (i + 1) depth (not inString) False
                 '{' | not inString -> findJsonEnd (i + 1) (depth + 1) inString False
                 '}' | not inString && depth > 0 -> 
                   if depth == 1
                   then i + 1  -- Encontrou o fim do objeto JSON raiz
                   else findJsonEnd (i + 1) (depth - 1) inString False
                 '[' | not inString -> findJsonEnd (i + 1) (depth + 1) inString False
                 ']' | not inString && depth > 0 -> 
                   if depth == 1
                   then i + 1  -- Encontrou o fim do array JSON raiz
                   else findJsonEnd (i + 1) (depth - 1) inString False
                 _ -> findJsonEnd (i + 1) depth inString False
  
  let jsonEnd = findJsonEnd 0 0 False False
  
  -- Pula whitespace após o JSON
  let rest = BL.drop jsonEnd bytes
  let restWithoutWhitespace = BLC.dropWhile (`elem` [' ', '\t', '\n', '\r']) rest
  
  Just (msg, restWithoutWhitespace)
