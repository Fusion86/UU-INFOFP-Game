{-# LANGUAGE TupleSections #-}

module Assets where

import Data.List (isSuffixOf)
import Data.Map (fromList, lookup)
import Data.Maybe (catMaybes)
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Juicy
import Model
import Rendering
import System.Directory (canonicalizePath, getDirectoryContents)
import System.FilePath (joinPath, takeBaseName, (</>))
import Prelude hiding (lookup)

-- Taken from https://stackoverflow.com/a/8572250
getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir =
  getDirectoryContents dir >>= mapM (canonicalizePath . (dir </>))

loadAssets :: FilePath -> IO Assets
loadAssets f = do
  files <- getAbsDirectoryContents f
  assetsMaybe <- mapM loadAsset (filter isAssetFile files)
  let assets = fromList (catMaybes assetsMaybe)
  return (trace ("Loaded assets: " ++ show assets) assets)
  where
    isAssetFile f = isSuffixOf "png" f

loadAsset :: FilePath -> IO (Maybe (String, Picture))
loadAsset f = fmap (takeBaseName f,) <$> loadJuicyPNG f -- tomsmeding

-- loadAsset f = do
--   picture <- loadJuicyPNG f

--   case picture of
--     Just picture -> return (Just (takeBaseName f, picture))
--     Nothing -> return Nothing

getAsset :: String -> Assets -> Picture
getAsset s a = case lookup s a of
  Just p -> p
  Nothing -> renderDbgString 0.25 red "!TEX"
