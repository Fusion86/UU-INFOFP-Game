{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Levels where

import Common
import Coordinates
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Text (Text, pack, splitOn, unpack)
import qualified Data.Text as T
import Model
import System.FilePath (takeBaseName)
import Text.XML.Light
import Utility

loadLevels :: FilePath -> IO [Level]
loadLevels f = do
  files <- getAbsDirectoryContents f
  maybeLevels <- mapM loadLevel (sort files)
  let levels = catMaybes maybeLevels
  return $
    trace ("Loaded " ++ show (length levels) ++ " levels.") $
      levels

loadLevel :: FilePath -> IO (Maybe Level)
loadLevel f = do
  xml <- trace ("Loading level from " ++ f) BS.readFile f
  -- Makes debugging easier
  let !eager = dbg "loadLevelFromXml" $ loadLevelFromXml xml
  return eager

loadLevelFromXml :: ByteString -> Maybe Level
loadLevelFromXml xml
  | Just name <- levelName =
    let !eagerLayers = layers
     in Just $ Level name levelBackground eagerLayers []
  | otherwise = Nothing
  where
    contents = parseXML xml
    simpleName s = QName s Nothing Nothing

    mapRoot :: Maybe Element
    mapRoot = safeHead $ concatMap (findElements $ simpleName "map") (onlyElems contents)

    mapProps :: Maybe Element
    mapProps = case mapRoot of
      Nothing -> trace "No map root found." Nothing
      Just x -> case findElement (simpleName "properties") x of
        Nothing -> trace "No map properties found" Nothing
        Just props -> Just props

    levelName :: Maybe String
    levelName = getPropValueByName mapProps "Name"

    levelBackground :: Maybe String
    levelBackground = getPropValueByName mapProps "Background"

    layers :: [TileLayer]
    layers = case mapRoot of
      Nothing -> trace "No map root found." []
      Just x -> mapMaybe parseLayerData $ findElements (simpleName "layer") x
      where
        parseLayerData :: Element -> Maybe TileLayer
        parseLayerData x
          -- TODO: Differentiate between SolidTileLayer, BackgroundTileLayer, and ForegroundTileLayer
          | Just layerData <- findElement (simpleName "data") x =
            let tiles = parseTileStr (pack (strContent layerData))
             in Just $ TileLayer BackgroundTileLayer tiles
          | otherwise = trace "Layer has no data." Nothing

        parseTileStr :: Text -> [Int]
        parseTileStr str = map (read . unpack) $ splitOn "," (T.filter filterChars str)

        -- Remove whitespaces from the string.
        filterChars :: Char -> Bool
        filterChars c
          | c == ' ' = False
          | c == '\n' = False
          | otherwise = True

    getPropValueByName :: Maybe Element -> String -> Maybe String
    getPropValueByName propsElement propName
      | Just props <- propsElement,
        Just nameProp <- filterChild f props,
        Just value <- findAttr (simpleName "value") nameProp =
        Just value
      | otherwise = Nothing
      where
        f :: Element -> Bool
        f x
          | Just name <- findAttr (simpleName "name") x = name == propName
          | otherwise = False

-- Returns the index of the invalid layer, or Nothing
validateLayers :: [TileLayer] -> Maybe Int
validateLayers = helper 0
  where
    expectedTileCount = round (worldWidth / 8 * worldHeight / 8)

    helper :: Int -> [TileLayer] -> Maybe Int
    helper _ [] = Nothing
    helper i (x : xs)
      | length (tiles x) /= expectedTileCount = Just i
      | otherwise = helper (i + 1) xs
