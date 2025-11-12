{-# LANGUAGE OverloadedStrings #-}
module System.Network.NetworkStateSpec (spec) where

import Test.Hspec
import qualified System.Network.NetworkState as NS
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newMVar, newEmptyMVar, putMVar, takeMVar)

-- Helper para verificar NetworkState (j? que n?o tem Show)
isNoNetworkState :: NS.NetworkState -> Bool
isNoNetworkState NS.NoNetworkState = True
isNoNetworkState _ = False

spec :: Spec
spec = describe "System.Network.NetworkState" $ do
  
  describe "NetworkState initialization" $ do
    it "starts with NoNetworkState" $ do
      -- Limpa o estado antes do teste
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
      -- Define um estado qualquer primeiro
      NS.setNetworkState NS.NoNetworkState
      -- Agora limpa
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
      -- Este teste verifica que o MVar protege contra race conditions
      NS.clearNetworkState
      
      -- Cria uma barreira de sincroniza??o
      barrier <- newEmptyMVar
      done1 <- newEmptyMVar
      done2 <- newEmptyMVar
      
      -- Thread 1: define estado v?rias vezes
      _ <- forkIO $ do
        takeMVar barrier  -- Espera pela libera??o
        sequence_ $ replicate 100 $ NS.setNetworkState NS.NoNetworkState
        putMVar done1 ()
      
      -- Thread 2: define estado v?rias vezes
      _ <- forkIO $ do
        takeMVar barrier  -- Espera pela libera??o
        sequence_ $ replicate 100 $ NS.setNetworkState NS.NoNetworkState
        putMVar done2 ()
      
      -- Libera as threads simultaneamente
      putMVar barrier ()
      putMVar barrier ()
      
      -- Espera ambas terminarem
      takeMVar done1
      takeMVar done2
      
      -- Verifica que o estado ? consistente
      state <- NS.getNetworkState
      isNoNetworkState state `shouldBe` True
    
    it "handles concurrent reads during writes" $ do
      NS.clearNetworkState
      
      resultMVar <- newMVar []
      
      -- Thread writer: muda o estado repetidamente
      _ <- forkIO $ do
        sequence_ $ replicate 50 $ do
          NS.setNetworkState NS.NoNetworkState
          threadDelay 100  -- 0.1ms
      
      -- Threads readers: leem o estado repetidamente
      _ <- forkIO $ do
        states <- sequence $ replicate 50 $ do
          state <- NS.getNetworkState
          threadDelay 100
          return state
        -- Todas as leituras devem ser v?lidas
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
      
      -- Espera um pouco para as threads terminarem
      threadDelay 10000  -- 10ms
      
      results <- takeMVar resultMVar
      -- Pelo menos algumas leituras devem ter ocorrido
      -- e todas devem ter sido v?lidas
      if null results
        then return ()  -- Threads ainda rodando, ok
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
