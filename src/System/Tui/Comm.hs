{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module System.Tui.Comm
  ( GameOutput(..)
  , MessageType(..)
  , ChoicePromptPayload(..)
  , ChoiceOptionPayload(..)
  , ChoiceSelectionPayload(..)
  ) where

import qualified Data.Text as T
import qualified System.GameContext as GameContext
import qualified System.ConsequenceContract as Consequence
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

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
  | ChoicePrompt ChoicePromptPayload

-- | Payload sent to TUI to request player input for branching consequences.
data ChoicePromptPayload = ChoicePromptPayload
  { choicePromptId :: T.Text
  , choicePromptTitle :: T.Text
  , choicePromptMessage :: T.Text
  , choicePromptOptions :: [ChoiceOptionPayload]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Individual option presented to the player in the popup.
data ChoiceOptionPayload = ChoiceOptionPayload
  { choiceOptionIndex :: Int
  , choiceOptionLabel :: T.Text
  , choiceOptionConsequences :: T.Text -- JSON-encoded list of consequences
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Payload returned by the TUI when the user resolves a choice.
data ChoiceSelectionPayload = ChoiceSelectionPayload
  { choiceSelectionPromptId :: T.Text
  , choiceSelectionSelectedIndex :: Int
  , choiceSelectionLabel :: T.Text
  , choiceSelectionConsequences :: [Consequence.Consequence]
  , choiceSelectionCancelled :: Bool
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)
