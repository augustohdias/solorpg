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


data MessageType
  = SystemMessage    
  | NarrativeMessage 
  deriving (Eq, Show)


data GameOutput
  = LogEntry T.Text MessageType
  | CharacterUpdate GameContext.MainCharacter
  | GameEnd
  | ChoicePrompt ChoicePromptPayload
  | ConnectionStatus T.Text Bool  
  | HostInfo T.Text T.Text        
  | AssetExploreRequest [GameContext.Asset]  
  | AssetViewRequest GameContext.Asset       
  | ConnectionRequestPrompt T.Text T.Text  
  | PlayerList [T.Text]            


data ChoicePromptPayload = ChoicePromptPayload
  { choicePromptId :: T.Text
  , choicePromptTitle :: T.Text
  , choicePromptMessage :: T.Text
  , choicePromptOptions :: [ChoiceOptionPayload]
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ChoiceOptionPayload = ChoiceOptionPayload
  { choiceOptionIndex :: Int
  , choiceOptionLabel :: T.Text
  , choiceOptionConsequences :: T.Text 
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


data ChoiceSelectionPayload = ChoiceSelectionPayload
  { choiceSelectionPromptId :: T.Text
  , choiceSelectionSelectedIndex :: Int
  , choiceSelectionLabel :: T.Text
  , choiceSelectionConsequences :: [Consequence.Consequence]
  , choiceSelectionCancelled :: Bool
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)
