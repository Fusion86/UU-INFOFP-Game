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
  return $ loadLevelFromXml xml

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
          | Just layerData <- findElement (simpleName "data") x,
            Just layerName <- findAttr (simpleName "name") x =
            let tiles = parseTileStr (pack (strContent layerData))
             in Just $ TileLayer (layerNameToType layerName) $ tilesToTileGrid tiles
          | otherwise = trace "Layer has no data." Nothing

        parseTileStr :: Text -> [Int]
        parseTileStr str = map (read . unpack) $ splitOn "," (T.filter filterChars str)

        -- Remove whitespaces from the string.
        filterChars :: Char -> Bool
        filterChars c
          | c == ' ' = False
          | c == '\n' = False
          | otherwise = True

        layerNameToType :: String -> TileLayerType
        layerNameToType x
          | x == "Foreground" = ForegroundTileLayer
          | x == "Solid" = SolidTileLayer
          | x == "Background" = BackgroundTileLayer
          | otherwise = trace ("Unknown layer type: " ++ x) BackgroundTileLayer

    getPropValueByName :: Maybe Element -> String -> Maybe String
    getPropValueByName propsElement propName
      | Just props <- propsElement,
        Just prop <- filterChild f props,
        Just value <- findAttr (simpleName "value") prop =
        Just value
      | otherwise = Nothing
      where
        f :: Element -> Bool
        f x
          | Just name <- findAttr (simpleName "name") x = name == propName
          | otherwise = False

tilesToTileGrid :: [Int] -> TileGrid
tilesToTileGrid = helper (0, 0)
  where
    helper :: Vec2 -> [Int] -> TileGrid
    helper _ [] = []
    helper (x, y) lst@(tile : tiles)
      -- If end of row, go to next row.
      | x >= worldWidth = helper (0, y + 8) lst
      -- If not a null tile
      | tile /= 0 = ((x, y), tile) : helper (x + 8, y) tiles
      | otherwise = helper (x + 8, y) tiles
