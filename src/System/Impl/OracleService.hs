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
import Control.Exception (try, IOException)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, readMVar)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)

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
  if not dirExists
    then putStrLn $ "Diretório de oráculos não encontrado: " ++ oraclesDir iHandle
    else do
      files <- try (listDirectory (oraclesDir iHandle)) :: IO (Either IOException [FilePath])
      case files of
        Left err -> putStrLn $ "Erro ao ler diretório: " ++ show err
        Right fileList -> do
          let jsonFiles = filter (\f -> takeExtension f == ".json") fileList
          putStrLn $ "Carregando " ++ show (length jsonFiles) ++ " oráculo(s)..."
          mapM_ (\f -> do
            result <- loadOracleImpl iHandle (oraclesDir iHandle </> f)
            case result of
              Left err -> putStrLn $ "Erro ao carregar " ++ f ++ ": " ++ show err
              Right oracle -> putStrLn $ "✓ " ++ T.unpack (Oracle.oracleName oracle)
            ) jsonFiles

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

-- | Consulta oráculo com valor específico
queryOracleImpl :: InternalHandle -> T.Text -> Int -> IO (Either Oracle.OracleError Oracle.OracleResult)
queryOracleImpl iHandle oracleName rollValue = do
  cache <- readMVar (oracleCache iHandle)
  case Map.lookup oracleName cache of
    Nothing -> return $ Left (Oracle.OracleNotFound oracleName)
    Just oracle -> 
      case findEntry rollValue (Oracle.oracleEntries oracle) of
        Nothing -> return $ Left (Oracle.InvalidRollValue rollValue)
        Just entry -> return $ Right $ Oracle.OracleResult
          { Oracle.resultOracle = oracleName
          , Oracle.resultRoll = rollValue
          , Oracle.resultText = Oracle.entryText entry
          , Oracle.resultConsequence = Oracle.entryConsequence entry
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
  case Map.lookup oracleName cache of
    Nothing -> return $ Left (Oracle.OracleNotFound oracleName)
    Just oracle -> do
      -- Rola o dado especificado no oráculo
      rolls <- Dice.roll (diceHandle iHandle) (Oracle.oracleDice oracle)
      case rolls of
        ((_, rollValue):_) -> queryOracleImpl iHandle oracleName rollValue
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
  case Map.lookup oracleName cache of
    Nothing -> return $ Left (Oracle.OracleNotFound oracleName)
    Just oracle -> return $ Right oracle

