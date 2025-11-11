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

-- | Entrada de oráculo (linha da tabela)
data OracleEntry = OracleEntry
  { entryRange :: !(Int, Int)           -- ^ Range de valores (ex: 1-10)
  , entryText :: !T.Text                -- ^ Texto do resultado
  , entryConsequences :: ![Consequence.Consequence] -- ^ Consequências estruturadas
  } deriving (Eq, Show, Generic, ToJSON)

instance FromJSON OracleEntry where
  parseJSON = withObject "OracleEntry" $ \v -> OracleEntry
    <$> v .: "entryRange"
    <*> v .: "entryText"
    <*> v .:? "entryConsequences" .!= []

-- | Tabela de oráculo
data Oracle = Oracle
  { oracleName :: !T.Text           -- ^ Nome do oráculo
  , oracleDescription :: !T.Text    -- ^ Descrição
  , oracleEntries :: ![OracleEntry] -- ^ Entradas da tabela
  , oracleDice :: !T.Text           -- ^ Dado usado (ex: "1d100")
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Resultado de consulta ao oráculo
data OracleResult = OracleResult
  { resultOracle :: !T.Text          -- ^ Nome do oráculo consultado
  , resultRoll :: !Int               -- ^ Valor rolado
  , resultText :: !T.Text            -- ^ Texto do resultado
  , resultConsequences :: ![Consequence.Consequence]  -- ^ Consequências estruturadas
  } deriving (Eq, Show)

-- | Erros do sistema de oráculos
data OracleError
  = OracleNotFound T.Text       -- ^ Oráculo não existe
  | InvalidRollValue Int        -- ^ Valor fora do range
  | EmptyOracle                 -- ^ Oráculo sem entradas
  | InvalidFormat String        -- ^ Erro no formato JSON
  deriving (Eq, Show)

-- Cache global de oráculos (usando MVar para thread-safety)
-- Inicializado na primeira chamada de qualquer função
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

-- | Inicializa oráculos a partir de um diretório
initializeOracles :: FilePath -> IO ()
initializeOracles oraclesDir = do
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

-- | Carrega oráculo de arquivo (versão interna que recebe cache)
loadOracleFile :: MVar (Map.Map T.Text Oracle) -> FilePath -> IO (Either OracleError Oracle)
loadOracleFile cache path = do
  result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
  case result of
    Left err -> return $ Left (InvalidFormat $ show err)
    Right contents ->
      case decode contents of
        Nothing -> return $ Left (InvalidFormat "JSON inválido")
        Just oracle -> do
          -- Adiciona ao cache
          modifyMVar cache $ \cacheMap ->
            return (Map.insert (oracleName oracle) oracle cacheMap, ())
          return $ Right oracle

-- | Carrega oráculo de arquivo (versão pública)
loadOracle :: FilePath -> IO (Either OracleError Oracle)
loadOracle path = do
  cache <- getCache
  loadOracleFile cache path

-- | Encontra oráculo no cache com busca case-insensitive
findOracleCaseInsensitive :: Map.Map T.Text Oracle -> T.Text -> Maybe (T.Text, Oracle)
findOracleCaseInsensitive cache searchName =
  -- Primeiro tenta correspondência exata case-insensitive
  case find (\(name, _) -> T.toCaseFold name == T.toCaseFold searchName) (Map.toList cache) of
    Just result -> Just result
    Nothing ->
      -- Se não encontrar, tenta correspondência parcial (contém)
      find (\(name, _) -> T.toCaseFold searchName `T.isInfixOf` T.toCaseFold name ||
                          T.toCaseFold name `T.isInfixOf` T.toCaseFold searchName) (Map.toList cache)

-- | Consulta oráculo com valor específico
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

-- | Encontra entrada que contém o valor
findEntry :: Int -> [OracleEntry] -> Maybe OracleEntry
findEntry _ [] = Nothing
findEntry val (entry:rest)
  | val >= fst (entryRange entry) && val <= snd (entryRange entry) = Just entry
  | otherwise = findEntry val rest

-- | Consulta oráculo com rolagem automática
rollOracle :: T.Text -> IO (Either OracleError OracleResult)
rollOracle oracleNameParam = do
  cache <- getCache
  cacheMap <- readMVar cache
  case findOracleCaseInsensitive cacheMap oracleNameParam of
    Nothing -> return $ Left (OracleNotFound oracleNameParam)
    Just (actualName, oracle) -> do
      -- Rola o dado especificado no oráculo
      rolls <- Dice.roll (oracleDice oracle)
      case rolls of
        ((_, rollValue):_) -> queryOracle actualName rollValue
        [] -> return $ Left EmptyOracle

-- | Lista oráculos carregados
listOracles :: IO [T.Text]
listOracles = do
  cache <- getCache
  cacheMap <- readMVar cache
  return $ Map.keys cacheMap

-- | Mostra oráculo completo
showOracle :: T.Text -> IO (Either OracleError Oracle)
showOracle oracleNameParam = do
  cache <- getCache
  cacheMap <- readMVar cache
  case findOracleCaseInsensitive cacheMap oracleNameParam of
    Nothing -> return $ Left (OracleNotFound oracleNameParam)
    Just (_, oracle) -> return $ Right oracle

