module Levels where

import Collision
import Common
import Coordinates
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Map (empty, fromList)
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
    trace ("Loaded " ++ show (length levels) ++ " levels.") levels

loadLevel :: FilePath -> IO (Maybe Level)
loadLevel f = do
  xml <- trace ("Loading level from " ++ f) BS.readFile f
  return $ loadLevelFromXml xml

loadLevelFromXml :: ByteString -> Maybe Level
loadLevelFromXml xml
  | Just name <- levelName =
    -- We want eager evaluation to show all warnings for all levels (and not just when loading one level).
    let !eagerLayers = layers
        !eagerObjects = levelObjects
     in Just $ Level name levelBackground levelForeground levelParallax eagerLayers eagerObjects
  | otherwise = Nothing
  where
    contents = parseXML xml
    simpleName s = QName s Nothing Nothing

    mapRoot :: Maybe Element
    mapRoot = safeHead $ concatMap (findElements $ simpleName "map") (onlyElems contents)

    mapProps :: Maybe Element
    mapProps = case mapRoot of
      -- We could use a Maybe monad here, but I like the verbose logging.
      Nothing -> trace "No map root found." Nothing
      Just x -> case findElement (simpleName "properties") x of
        Nothing -> trace "No map properties found" Nothing
        Just props -> Just props

    objectGroup :: Maybe Element
    objectGroup = case mapRoot of
      -- We could use a Maybe monad here, but I like the verbose logging.
      Nothing -> trace "No map root found." Nothing
      Just x -> case findElement (simpleName "objectgroup") x of
        Nothing -> trace "No objectgroup found" Nothing
        Just y -> Just y

    levelObjects :: [LevelObject]
    levelObjects = case objectGroup of
      Nothing -> trace "No objectGroup found." []
      Just x -> mapMaybe parseLevelObject $ findElements (simpleName "object") x
      where
        parseLevelObject :: Element -> Maybe LevelObject
        parseLevelObject x = do
          objectTypeStr <- findAttr (simpleName "name") x
          objectType <- parseObjectType objectTypeStr
          posX <- findAttr (simpleName "x") x
          posY <- findAttr (simpleName "y") x
          return $ LevelObject objectType (readFloat posX, readFloat posY) (width, height) levelObjectProperties
          where
            parseObjectType :: String -> Maybe LevelObjectType
            parseObjectType "Collision" = Just CollisionObject
            parseObjectType "EnemyCollision" = Just EnemyCollisionObject
            parseObjectType "Death" = Just DeathObject
            parseObjectType "Damage" = Just DamageObject
            parseObjectType "PlayerSpawn" = Just PlayerSpawnObject
            parseObjectType "EnemySpawner" = Just EnemySpawnerObject
            parseObjectType "LevelEnd" = Just LevelEndObject
            parseObjectType str = trace ("Unknown object type '" ++ str ++ "'") Nothing

            readFloat y = read y :: Float

            -- Width and height are optional properties, when they are not found they will be zero.
            width
              | Just str <- findAttr (simpleName "width") x = readFloat str
              | otherwise = 0
            height
              | Just str <- findAttr (simpleName "height") x = readFloat str
              | otherwise = 0

            levelObjectProperties :: LevelObjectProperties
            levelObjectProperties
              | Just props <- findElement (simpleName "properties") x = fromList $ mapMaybe f (elChildren props)
              | otherwise = empty
              where
                f :: Element -> Maybe (LevelObjectProperty, String)
                f x = do
                  name <- findAttr (simpleName "name") x
                  propType <- parseLevelObjectProperty name
                  value <- findAttr (simpleName "value") x
                  return (propType, value)

            parseLevelObjectProperty :: String -> Maybe LevelObjectProperty
            parseLevelObjectProperty "SpawnChance" = Just SpawnChance
            parseLevelObjectProperty "NextLevel" = Just NextLevel
            parseLevelObjectProperty "Type" = Just TypeProperty
            parseLevelObjectProperty _ = Nothing

    levelName :: Maybe String
    levelName = getPropValueByName mapProps "Name"

    levelBackground :: Maybe String
    levelBackground = getPropValueByName mapProps "Background"

    levelForeground :: Maybe String
    levelForeground = getPropValueByName mapProps "Foreground"

    levelParallax :: Maybe String
    levelParallax = getPropValueByName mapProps "Parallax"

    layers :: [TileLayer]
    layers = case mapRoot of
      Nothing -> trace "No map root found." []
      Just x -> mapMaybe parseLayerData $ findElements (simpleName "layer") x
      where
        parseLayerData :: Element -> Maybe TileLayer
        parseLayerData x
          | Just layerData <- findElement (simpleName "data") x,
            Just layerName <- findAttr (simpleName "name") x,
            Just layerType <- layerNameToType layerName =
            let tiles = parseTileStr (pack (strContent layerData))
             in Just $ TileLayer layerType $ tilesToTileGrid tiles
          | otherwise = trace "Layer has no data." Nothing

        parseTileStr :: Text -> [Int]
        parseTileStr str = map (read . unpack) $ splitOn "," (T.filter filterChars str)

        -- Remove whitespaces from the string.
        filterChars :: Char -> Bool
        filterChars c
          | c == ' ' = False
          | c == '\n' = False
          | otherwise = True

        layerNameToType :: String -> Maybe TileLayerType
        layerNameToType x
          | x == "Foreground" = Just ForegroundTileLayer
          | x == "Background" = Just BackgroundTileLayer
          | otherwise = Nothing

    getPropValueByName :: Maybe Element -> String -> Maybe String
    getPropValueByName propsElement propName = do
      props <- propsElement
      prop <- filterChild f props
      findAttr (simpleName "value") prop
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
      | x >= gameWidth = helper (0, y + 8) lst
      -- If not a null tile
      | tile /= 0 = ((x, y), tile) : helper (x + 8, y) tiles
      | otherwise = helper (x + 8, y) tiles
