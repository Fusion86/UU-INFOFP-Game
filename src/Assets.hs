{-# LANGUAGE TupleSections #-}

module Assets where

import Common
import Data.List (isSuffixOf)
import Data.Map (fromList, lookup)
import Data.Maybe (catMaybes)
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Model
import System.FilePath (takeBaseName)
import Utility
import Prelude hiding (lookup)

loadAssets :: FilePath -> IO Assets
loadAssets f = do
  files <- getAbsDirectoryContents f
  assetsMaybe <- mapM loadAsset (filter isAssetFile files)
  let assets = fromList (catMaybes assetsMaybe)
  return (trace ("Loaded assets: " ++ show assets) assets)
  where
    isAssetFile = isSuffixOf "png"

loadAsset :: FilePath -> IO (Maybe (String, Picture))
loadAsset f = fmap (takeBaseName f,) <$> loadJuicyPNG f -- tomsmeding

-- loadAsset f = do
--   picture <- loadJuicyPNG f

--   case picture of
--     Just picture -> return (Just (takeBaseName f, picture))
--     Nothing -> return Nothing

getAsset :: Assets -> String -> Picture
getAsset a s = case lookup s a of
  Just p -> p
  Nothing -> renderDbgString red "!TEX"
