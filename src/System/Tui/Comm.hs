module System.Tui.Comm (GameOutput(..), MessageType(..)) where

import qualified Data.Text as T
import qualified System.GameContextContract as GameContext

-- | Type of message to display
data MessageType
  = SystemMessage    -- System notifications (loaded, updated, etc.)
  | NarrativeMessage -- Story logs and game narrative
  deriving (Eq, Show)

-- | Defines the types of messages that the game engine can send to the TUI.
data GameOutput
  = LogEntry T.Text MessageType
  | CharacterUpdate GameContext.MainCharacter
  | GameEnd
