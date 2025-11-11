{-# LANGUAGE OverloadedStrings #-}

module MainLoop (runGameLoop) where

import Control.Concurrent.STM (TChan, readTChan)
import Control.Monad.STM (atomically)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified System.Action as Action
import System.Exit (exitSuccess)
import System.Tui.Comm (GameOutput)

runGameLoop :: TChan GameOutput -> TChan T.Text -> IO ()
runGameLoop outputChan inputChan = do
  -- Game loop
  let loop = do
        input <- atomically $ readTChan inputChan
        let (action, params) = parseCommand input

        continue <- Action.process outputChan action params
        if continue
          then loop
          else exitSuccess -- Or send a GameEnd message
  loop

parseCommand :: T.Text -> (Action.ActionType, T.Text)
parseCommand input =
  let text = T.strip input
   in if T.isPrefixOf ":" text
        then
          let (cmd, params) = T.breakOn " " text
           in (parseActionType (T.map toLower cmd), T.strip params)
        else (Action.AddStoryLog, text)

parseActionType :: T.Text -> Action.ActionType
parseActionType cmd = case cmd of
  ":r" -> Action.RollDice
  ":roll" -> Action.RollDice
  ":exit" -> Action.Exit
  ":q" -> Action.Exit
  ":quit" -> Action.Exit
  ":show" -> Action.Show
  ":create" -> Action.CreateCharacter
  ":load" -> Action.LoadCharacter
  ":char" -> Action.ShowCharacter
  ":setattr" -> Action.UpdateAttribute
  ":setres" -> Action.UpdateResource
  ":addattr" -> Action.AddAttribute
  ":addres" -> Action.AddResource
  ":challenge" -> Action.Challenge
  ":move" -> Action.Move
  ":vow" -> Action.SwearVow
  ":combat" -> Action.CreateCombatTrack
  ":progress" -> Action.MarkProgress
  ":fulfill" -> Action.RollProgress
  ":tracks" -> Action.ShowTracks
  ":abandon" -> Action.AbandonTrack
  ":oracle" -> Action.Oracle
  ":help" -> Action.Help
  ":bond" -> Action.Bond
  ":bonus" -> Action.AddBonusManually
  ":choice" -> Action.ResolveChoice
  _ -> Action.Unknown
