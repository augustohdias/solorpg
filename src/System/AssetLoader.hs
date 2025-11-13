{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{- | AssetLoader - Carrega definições de Assets do Ironsworn de arquivos JSON
     
     Este módulo é responsável por:
     - Carregar todos os assets da pasta assets/
     - Parsear JSON para estruturas Haskell
     - Manter cache global de assets disponíveis
     - Converter entre diferentes formatos de asset
-}
module System.AssetLoader
  ( 
    loadAllAssets
  , getAvailableAssets
  , findAssetByName
  , assetExists
  , AssetCache
  , getAssetCache
  
    
  , parseAssetFromJSON
  , AssetJSON (..)
  , AssetSkillJSON (..)
  ) where

import qualified System.GameContext as GameContext
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, parseJSON, withObject, (.:), (.:?), decode)
import Control.Exception (try, IOException)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (catMaybes)
import Data.List (find)
import qualified Data.ByteString.Lazy as BL
import Paths_SoloRPG (getDataDir)


data AssetSkillJSON = AssetSkillJSON
  { index :: !Int
  , skillName :: !T.Text
  , description :: !T.Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)


data AssetJSON = AssetJSON
  { jsonType :: !T.Text          
  , jsonName :: !T.Text          
  , skills :: ![AssetSkillJSON]  
  , enbaledSkills :: !(Maybe [Int]) 
  } deriving (Show, Eq, Generic)


instance FromJSON AssetJSON where
  parseJSON = withObject "AssetJSON" $ \o -> AssetJSON
    <$> o .: "type"
    <*> o .: "name"
    <*> o .: "skills"
    <*> o .:? "enbaledSkills" 


type AssetCache = MVar [GameContext.Asset]


{-# NOINLINE assetCache #-}
assetCache :: AssetCache
assetCache = unsafePerformIO $ newMVar []


getAssetCache :: IO AssetCache
getAssetCache = return assetCache


parseAssetType :: T.Text -> GameContext.AssetType
parseAssetType txt = case T.toUpper txt of
  "COMPANION" -> GameContext.Companion
  "PATH" -> GameContext.Path
  "COMBAT_TALENT" -> GameContext.CombatTalent
  "RITUAL" -> GameContext.Ritual
  "MODULE" -> GameContext.Module
  "SUPPORT_VEHICLE" -> GameContext.SupportVehicle
  "COMMAND_VEHICLE" -> GameContext.CommandVehicle
  "DEED" -> GameContext.Deed
  _ -> GameContext.Ritual 


parseAssetFromJSON :: AssetJSON -> GameContext.Asset
parseAssetFromJSON assetJson = GameContext.Asset
  { GameContext.assetType = parseAssetType (jsonType assetJson)
  , GameContext.assetName = jsonName assetJson
  , GameContext.assetSkills = map convertSkill (skills assetJson)
  }
  where
    convertSkill :: AssetSkillJSON -> GameContext.AssetSkill
    convertSkill skill = GameContext.AssetSkill
      { GameContext.skillIndex = index skill
      , GameContext.skillName = skillName skill
      , GameContext.skillDescription = description skill
      }


loadAssetFromFile :: FilePath -> IO (Maybe GameContext.Asset)
loadAssetFromFile filePath = do
  result <- try (BL.readFile filePath) :: IO (Either IOException BL.ByteString)
  case result of
    Left _ -> return Nothing
    Right content -> case decode content of
      Nothing -> return Nothing
      Just assetJson -> return $ Just (parseAssetFromJSON assetJson)


loadAllAssets :: IO [GameContext.Asset]
loadAllAssets = do
  dataDir <- getDataDir
  let assetsDir = dataDir </> "assets"
  exists <- doesDirectoryExist assetsDir
  if not exists
    then return []
    else do
      files <- listDirectory assetsDir
      let jsonFiles = filter (\f -> takeExtension f == ".json") files
      let fullPaths = map (assetsDir </>) jsonFiles
      maybeAssets <- mapM loadAssetFromFile fullPaths
      let assets = catMaybes maybeAssets
      
      
      modifyMVar_ assetCache $ \_ -> return assets
      
      return assets


getAvailableAssets :: IO [GameContext.Asset]
getAvailableAssets = readMVar assetCache


findAssetByName :: T.Text -> IO (Maybe GameContext.Asset)
findAssetByName name = do
  assets <- getAvailableAssets
  return $ find (\asset -> GameContext.assetName asset == name) assets


assetExists :: T.Text -> IO Bool
assetExists name = do
  maybeAsset <- findAssetByName name
  return $ case maybeAsset of
    Nothing -> False
    Just _ -> True
