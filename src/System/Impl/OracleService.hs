{-# LANGUAGE OverloadedStrings #-}
{- | Implementação do serviço de Oráculos
     
     Carrega oráculos de arquivos JSON no formato:
     {
       "name": "Nome do Oráculo",
       "description": "Descrição",
       "dice": "1d100",
       "entries": [
         {"range": [1, 10], "text": "Resultado 1-10"},
         {"range": [11, 20], "text": "Resultado 11-20"}
       ]
     }
-}
module System.Impl.OracleService (newHandle) where

import qualified System.OracleContract as Oracle
import qualified System.DiceContract as Dice
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Aeson (decode)
import Data.Either (partitionEithers)
import Data.Foldable (find)
import Control.Exception (try, IOException)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (when, unless)

-- | Handle interno com cache de oráculos
data InternalHandle = InternalHandle
  { oracleCache :: MVar (Map.Map T.Text Oracle.Oracle)
  , oraclesDir :: FilePath
  , diceHandle :: Dice.Handle
  }

-- | Cria novo handle para o serviço de Oráculos
newHandle :: FilePath -> Dice.Handle -> IO Oracle.Handle
newHandle oraclesPath diceH = do
  cache <- newMVar Map.empty
  let iHandle = InternalHandle
        { oracleCache = cache
        , oraclesDir = oraclesPath
        , diceHandle = diceH
        }

  -- Carrega oráculos automaticamente
  _ <- loadAllOracles iHandle

  return $ Oracle.Handle
    { Oracle.loadOracle = loadOracleImpl iHandle
    , Oracle.queryOracle = queryOracleImpl iHandle
    , Oracle.rollOracle = rollOracleImpl iHandle
    , Oracle.listOracles = listOraclesImpl iHandle
    , Oracle.showOracle = showOracleImpl iHandle
    }

-- | Carrega todos os oráculos do diretório
loadAllOracles :: InternalHandle -> IO ()
loadAllOracles iHandle = do
  dirExists <- doesDirectoryExist (oraclesDir iHandle)
  when dirExists $ do
    (try (listDirectory (oraclesDir iHandle)) :: IO (Either IOException [FilePath])) >>= either
      (const $ return ())
      (loadJsonFiles . filter ((== ".json") . takeExtension))
  where
    loadJsonFiles files = do
      results <- mapM (loadOracleImpl iHandle . (oraclesDir iHandle </>)) files
      let (errors, successes) = partitionEithers results
      unless (null errors) $ do
        putStrLn $ "Aviso: " ++ show (length errors) ++ " oráculo(s) falharam ao carregar"
        mapM_ (putStrLn . showOracleError) errors
      putStrLn $ "Carregados " ++ show (length successes) ++ " oráculo(s) com sucesso"

    showOracleError (Oracle.InvalidFormat msg) = "  Erro de formato: " ++ msg
    showOracleError (Oracle.OracleNotFound name) = "  Oráculo não encontrado: " ++ T.unpack name
    showOracleError (Oracle.InvalidRollValue val) = "  Valor inválido: " ++ show val
    showOracleError Oracle.EmptyOracle = "  Oráculo vazio"

-- | Carrega oráculo de arquivo
loadOracleImpl :: InternalHandle -> FilePath -> IO (Either Oracle.OracleError Oracle.Oracle)
loadOracleImpl iHandle path = do
  result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
  case result of
    Left err -> return $ Left (Oracle.InvalidFormat $ show err)
    Right contents ->
      case decode contents of
        Nothing -> return $ Left (Oracle.InvalidFormat "JSON inválido")
        Just oracle -> do
          -- Adiciona ao cache
          modifyMVar (oracleCache iHandle) $ \cache ->
            return (Map.insert (Oracle.oracleName oracle) oracle cache, ())
          return $ Right oracle

-- | Encontra oráculo no cache com busca case-insensitive e tolerante a acentos
findOracleCaseInsensitive :: Map.Map T.Text Oracle.Oracle -> T.Text -> Maybe (T.Text, Oracle.Oracle)
findOracleCaseInsensitive cache searchName =
  -- Primeiro tenta correspondência exata case-insensitive
  case find (\(name, _) -> T.toCaseFold name == T.toCaseFold searchName) (Map.toList cache) of
    Just result -> Just result
    Nothing ->
      -- Se não encontrar, tenta correspondência parcial (contém)
      find (\(name, _) -> T.toCaseFold searchName `T.isInfixOf` T.toCaseFold name ||
                          T.toCaseFold name `T.isInfixOf` T.toCaseFold searchName) (Map.toList cache)

-- | Consulta oráculo com valor específico
queryOracleImpl :: InternalHandle -> T.Text -> Int -> IO (Either Oracle.OracleError Oracle.OracleResult)
queryOracleImpl iHandle oracleName rollValue = do
  cache <- readMVar (oracleCache iHandle)
  case findOracleCaseInsensitive cache oracleName of
    Nothing -> return $ Left (Oracle.OracleNotFound oracleName)
    Just (actualName, oracle) ->
      case findEntry rollValue (Oracle.oracleEntries oracle) of
        Nothing -> return $ Left (Oracle.InvalidRollValue rollValue)
        Just entry -> return $ Right $ Oracle.OracleResult
          { Oracle.resultOracle = actualName
          , Oracle.resultRoll = rollValue
          , Oracle.resultText = Oracle.entryText entry
          , Oracle.resultConsequence = Oracle.entryConsequence entry
          , Oracle.resultConsequences = Oracle.entryConsequences entry
          }

-- | Encontra entrada que contém o valor
findEntry :: Int -> [Oracle.OracleEntry] -> Maybe Oracle.OracleEntry
findEntry _ [] = Nothing
findEntry val (entry:rest)
  | val >= fst (Oracle.entryRange entry) && val <= snd (Oracle.entryRange entry) = Just entry
  | otherwise = findEntry val rest

-- | Consulta oráculo com rolagem automática
rollOracleImpl :: InternalHandle -> T.Text -> IO (Either Oracle.OracleError Oracle.OracleResult)
rollOracleImpl iHandle oracleName = do
  cache <- readMVar (oracleCache iHandle)
  case findOracleCaseInsensitive cache oracleName of
    Nothing -> return $ Left (Oracle.OracleNotFound oracleName)
    Just (actualName, oracle) -> do
      -- Rola o dado especificado no oráculo
      rolls <- Dice.roll (diceHandle iHandle) (Oracle.oracleDice oracle)
      case rolls of
        ((_, rollValue):_) -> queryOracleImpl iHandle actualName rollValue
        [] -> return $ Left Oracle.EmptyOracle

-- | Lista oráculos carregados
listOraclesImpl :: InternalHandle -> IO [T.Text]
listOraclesImpl iHandle = do
  cache <- readMVar (oracleCache iHandle)
  return $ Map.keys cache

-- | Mostra oráculo completo
showOracleImpl :: InternalHandle -> T.Text -> IO (Either Oracle.OracleError Oracle.Oracle)
showOracleImpl iHandle oracleName = do
  cache <- readMVar (oracleCache iHandle)
  case findOracleCaseInsensitive cache oracleName of
    Nothing -> return $ Left (Oracle.OracleNotFound oracleName)
    Just (_, oracle) -> return $ Right oracle

