module Types (MessageType (..), MessageChannel (..)) where

data MessageType = Success | Error | Warning

data MessageChannel = SystemMessage | NarrativeMessage | PopUpMessage