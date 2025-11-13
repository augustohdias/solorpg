{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module System.Oracle
  ( Oracle (..)
  , OracleEntry (..)
  , OracleResult (..)
  , OracleError (..)
  , loadOracle
  , queryOracle
  , rollOracle
  , listOracles
  , showOracle
  , initializeOracles
  ) where

import qualified System.Dice as Dice
import qualified System.ConsequenceContract as Consequence
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Aeson (decode, ToJSON, FromJSON, parseJSON, (.:), (.:?), (.!=), withObject)
import Data.Either (partitionEithers)
import Data.Foldable (find)
import Control.Exception (try, IOException)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (when, unless)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import Paths_SoloRPG (getDataDir)


data OracleEntry = OracleEntry
  { entryRange :: !(Int, Int)           
  , entryText :: !T.Text                
  , entryConsequences :: ![Consequence.Consequence] 
  } deriving (Eq, Show, Generic, ToJSON)

instance FromJSON OracleEntry where
  parseJSON = withObject "OracleEntry" $ \v -> OracleEntry
    <$> v .: "entryRange"
    <*> v .: "entryText"
    <*> v .:? "entryConsequences" .!= []


data Oracle = Oracle
  { oracleName :: !T.Text           
  , oracleDescription :: !T.Text    
  , oracleEntries :: ![OracleEntry] 
  , oracleDice :: !T.Text           
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)


data OracleResult = OracleResult
  { resultOracle :: !T.Text          
  , resultRoll :: !Int               
  , resultText :: !T.Text            
  , resultConsequences :: ![Consequence.Consequence]  
  } deriving (Eq, Show)


data OracleError
  = OracleNotFound T.Text       
  | InvalidRollValue Int        
  | EmptyOracle                 
  | InvalidFormat String        
  deriving (Eq, Show)



oracleCacheRef :: MVar (Maybe (MVar (Map.Map T.Text Oracle)))
oracleCacheRef = unsafePerformIO (newMVar Nothing)
{-# NOINLINE oracleCacheRef #-}

getCache :: IO (MVar (Map.Map T.Text Oracle))
getCache = do
  maybeCache <- readMVar oracleCacheRef
  case maybeCache of
    Just cache -> return cache
    Nothing -> do
      cache <- newMVar Map.empty
      modifyMVar_ oracleCacheRef (const (return (Just cache)))
      return cache


initializeOracles :: FilePath -> IO ()
initializeOracles _ = do
  dataDir <- getDataDir
  let oraclesDir = dataDir </> "oracles"
  dirExists <- doesDirectoryExist oraclesDir
  when dirExists $ do
    (try (listDirectory oraclesDir) :: IO (Either IOException [FilePath])) >>= either
      (const $ return ())
      (loadJsonFiles oraclesDir . filter ((== ".json") . takeExtension))
  where
    loadJsonFiles dir files = do
      cache <- getCache
      results <- mapM (\f -> loadOracleFile cache (dir </> f)) files
      let (errors, successes) = partitionEithers results
      unless (null errors) $ do
        putStrLn $ "Aviso: " ++ show (length errors) ++ " oráculo(s) falharam ao carregar"
        mapM_ (putStrLn . showOracleError) errors
      putStrLn $ "Carregados " ++ show (length successes) ++ " oráculo(s) com sucesso"

    showOracleError (InvalidFormat msg) = "  Erro de formato: " ++ msg
    showOracleError (OracleNotFound name) = "  Oráculo não encontrado: " ++ T.unpack name
    showOracleError (InvalidRollValue val) = "  Valor inválido: " ++ show val
    showOracleError EmptyOracle = "  Oráculo vazio"


loadOracleFile :: MVar (Map.Map T.Text Oracle) -> FilePath -> IO (Either OracleError Oracle)
loadOracleFile cache path = do
  result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
  case result of
    Left err -> return $ Left (InvalidFormat $ show err)
    Right contents ->
      case decode contents of
        Nothing -> return $ Left (InvalidFormat "JSON inválido")
        Just oracle -> do
          
          modifyMVar cache $ \cacheMap ->
            return (Map.insert (oracleName oracle) oracle cacheMap, ())
          return $ Right oracle


loadOracle :: FilePath -> IO (Either OracleError Oracle)
loadOracle path = do
  cache <- getCache
  loadOracleFile cache path


findOracleCaseInsensitive :: Map.Map T.Text Oracle -> T.Text -> Maybe (T.Text, Oracle)
findOracleCaseInsensitive cache searchName =
  
  case find (\(name, _) -> T.toCaseFold name == T.toCaseFold searchName) (Map.toList cache) of
    Just result -> Just result
    Nothing ->
      
      find (\(name, _) -> T.toCaseFold searchName `T.isInfixOf` T.toCaseFold name ||
                          T.toCaseFold name `T.isInfixOf` T.toCaseFold searchName) (Map.toList cache)


queryOracle :: T.Text -> Int -> IO (Either OracleError OracleResult)
queryOracle oracleNameParam rollValue = do
  cache <- getCache
  cacheMap <- readMVar cache
  case findOracleCaseInsensitive cacheMap oracleNameParam of
    Nothing -> return $ Left (OracleNotFound oracleNameParam)
    Just (actualName, oracle) ->
      case findEntry rollValue (oracleEntries oracle) of
        Nothing -> return $ Left (InvalidRollValue rollValue)
        Just entry -> return $ Right $ OracleResult
          { resultOracle = actualName
          , resultRoll = rollValue
          , resultText = entryText entry
          , resultConsequences = entryConsequences entry
          }


findEntry :: Int -> [OracleEntry] -> Maybe OracleEntry
findEntry _ [] = Nothing
findEntry val (entry:rest)
  | val >= fst (entryRange entry) && val <= snd (entryRange entry) = Just entry
  | otherwise = findEntry val rest


rollOracle :: T.Text -> IO (Either OracleError OracleResult)
rollOracle oracleNameParam = do
  cache <- getCache
  cacheMap <- readMVar cache
  case findOracleCaseInsensitive cacheMap oracleNameParam of
    Nothing -> return $ Left (OracleNotFound oracleNameParam)
    Just (actualName, oracle) -> do
      
      rolls <- Dice.roll (oracleDice oracle)
      case rolls of
        ((_, rollValue):_) -> queryOracle actualName rollValue
        [] -> return $ Left EmptyOracle


listOracles :: IO [T.Text]
listOracles = do
  cache <- getCache
  cacheMap <- readMVar cache
  return $ Map.keys cacheMap


showOracle :: T.Text -> IO (Either OracleError Oracle)
showOracle oracleNameParam = do
  cache <- getCache
  cacheMap <- readMVar cache
  case findOracleCaseInsensitive cacheMap oracleNameParam of
    Nothing -> return $ Left (OracleNotFound oracleNameParam)
    Just (_, oracle) -> return $ Right oracle

