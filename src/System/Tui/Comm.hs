module System.Tui.Comm (GameOutput(..)) where

import qualified Data.Text as T
import qualified System.GameContextContract as GameContext

-- | Defines the types of messages that the game engine can send to the TUI.
data GameOutput
  = LogEntry T.Text
  | CharacterUpdate GameContext.MainCharacter
  | GameEnd
