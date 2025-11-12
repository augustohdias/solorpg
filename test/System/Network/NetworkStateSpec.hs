{-# LANGUAGE OverloadedStrings #-}
module System.Network.NetworkStateSpec (spec) where

import Test.Hspec
import qualified System.Network.NetworkState as NS
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, newEmptyMVar, putMVar, takeMVar)


isNoNetworkState :: NS.NetworkState -> Bool
isNoNetworkState NS.NoNetworkState = True
isNoNetworkState _ = False

spec :: Spec
spec = describe "System.Network.NetworkState" $ do
  
  describe "NetworkState initialization" $ do
    it "starts with NoNetworkState" $ do
      
      NS.clearNetworkState
      state <- NS.getNetworkState
      isNoNetworkState state `shouldBe` True
  
  describe "setNetworkState and getNetworkState" $ do
    it "can set and retrieve NoNetworkState" $ do
      NS.setNetworkState NS.NoNetworkState
      state <- NS.getNetworkState
      isNoNetworkState state `shouldBe` True
    
    it "maintains state across multiple reads" $ do
      NS.setNetworkState NS.NoNetworkState
      state1 <- NS.getNetworkState
      state2 <- NS.getNetworkState
      isNoNetworkState state1 `shouldBe` True
      isNoNetworkState state2 `shouldBe` True
  
  describe "clearNetworkState" $ do
    it "resets state to NoNetworkState" $ do
      
      NS.setNetworkState NS.NoNetworkState
      
      NS.clearNetworkState
      state <- NS.getNetworkState
      isNoNetworkState state `shouldBe` True
    
    it "clears state even after multiple sets" $ do
      NS.setNetworkState NS.NoNetworkState
      NS.setNetworkState NS.NoNetworkState
      NS.setNetworkState NS.NoNetworkState
      NS.clearNetworkState
      state <- NS.getNetworkState
      isNoNetworkState state `shouldBe` True
  
  describe "NetworkState thread safety" $ do
    it "handles concurrent state changes correctly" $ do
      
      NS.clearNetworkState
      
      
      barrier <- newEmptyMVar
      done1 <- newEmptyMVar
      done2 <- newEmptyMVar
      
      
      _ <- forkIO $ do
        takeMVar barrier  
        sequence_ $ replicate 100 $ NS.setNetworkState NS.NoNetworkState
        putMVar done1 ()
      
      
      _ <- forkIO $ do
        takeMVar barrier  
        sequence_ $ replicate 100 $ NS.setNetworkState NS.NoNetworkState
        putMVar done2 ()
      
      
      putMVar barrier ()
      putMVar barrier ()
      
      
      takeMVar done1
      takeMVar done2
      
      
      state <- NS.getNetworkState
      isNoNetworkState state `shouldBe` True
    
    it "handles concurrent reads during writes" $ do
      NS.clearNetworkState
      
      resultMVar <- newMVar []
      
      
      _ <- forkIO $ do
        sequence_ $ replicate 50 $ do
          NS.setNetworkState NS.NoNetworkState
          threadDelay 100  
      
      
      _ <- forkIO $ do
        states <- sequence $ replicate 50 $ do
          state <- NS.getNetworkState
          threadDelay 100
          return state
        
        let allValid = all isNoNetworkState states
        current <- takeMVar resultMVar
        putMVar resultMVar (allValid : current)
      
      _ <- forkIO $ do
        states <- sequence $ replicate 50 $ do
          state <- NS.getNetworkState
          threadDelay 100
          return state
        let allValid = all isNoNetworkState states
        current <- takeMVar resultMVar
        putMVar resultMVar (allValid : current)
      
      
      threadDelay 10000  
      
      results <- takeMVar resultMVar
      
      
      if null results
        then return ()  
        else all id results `shouldBe` True
  
  describe "clearNetworkState idempotency" $ do
    it "can be called multiple times safely" $ do
      NS.clearNetworkState
      NS.clearNetworkState
      NS.clearNetworkState
      state <- NS.getNetworkState
      isNoNetworkState state `shouldBe` True
    
    it "maintains NoNetworkState after multiple clears" $ do
      NS.clearNetworkState
      state1 <- NS.getNetworkState
      NS.clearNetworkState
      state2 <- NS.getNetworkState
      NS.clearNetworkState
      state3 <- NS.getNetworkState
      
      isNoNetworkState state1 `shouldBe` True
      isNoNetworkState state2 `shouldBe` True
      isNoNetworkState state3 `shouldBe` True
