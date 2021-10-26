module Assets where

import Codec.Picture (convertRGBA8, readImage)
import Codec.Picture.Extra (crop)
import Colors
import Common
import Data.List (isSuffixOf)
import Data.Map (fromList, lookup)
import Data.Maybe (catMaybes)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Juicy (fromImageRGBA8, loadJuicyPNG)
import Model
import System.FilePath (takeBaseName, (</>))
import Utility
import Prelude hiding (lookup)

loadAssets :: FilePath -> IO Assets
loadAssets f = do
  -- Load image assets
  files <- getAbsDirectoryContents (f </> "images")
  assetsMaybe <- mapM loadImageAsset (filter isAssetFile files)
  let assets = fromList (catMaybes assetsMaybe)

  -- Load character sheets
  playerCharacterSheet <- loadPlayerCharacterSheet (f </> "PlayerCharacterSheet.png")
  fxCharacterSheet <- loadFxCharacterSheet (f </> "FxSheet.png")
  enemyCharacterSheet <- loadEnemyCharacterSheet (f </> "ObjectsAndEnemies.png")

  return $ dbg "Loaded assets" $ Assets assets playerCharacterSheet fxCharacterSheet enemyCharacterSheet
  where
    isAssetFile = isSuffixOf "png"

loadImageAsset :: FilePath -> IO (Maybe (String, Picture))
loadImageAsset f = fmap (takeBaseName f,) <$> loadJuicyPNG f -- tomsmeding

-- loadAsset f = do
--   picture <- loadJuicyPNG f

--   case picture of
--     Just picture -> return (Just (takeBaseName f, picture))
--     Nothing -> return Nothing

loadPlayerCharacterSheet :: FilePath -> IO PlayerCharacterSheet
loadPlayerCharacterSheet f = do
  x <- readImage f
  case x of
    Left err -> error err
    Right dynImg ->
      return $
        PlayerCharacterSheet
          playerIdleRight
          playerIdleLeft
          playerWalkRight
          playerWalkLeft
      where
        img = convertRGBA8 dynImg
        f :: Int -> Int -> Picture
        f x y = fromImageRGBA8 $ crop x y 34 24 img

        -- Sprite defs
        playerIdleRight = f 1 1
        playerIdleLeft = f 1 27
        playerWalkRight = map (`f` 53) [1, 37 .. 253]
        playerWalkLeft = map (`f` 79) [1, 37 .. 253]

loadFxCharacterSheet :: FilePath -> IO FxSheet
loadFxCharacterSheet f = do
  x <- readImage f
  case x of
    Left err -> error err
    Right dynImg ->
      return $
        FxSheet
          playerBulletImpact
      where
        img = convertRGBA8 dynImg
        f :: Int -> Int -> Int -> Int -> Picture
        f x y w h = fromImageRGBA8 $ crop x y w h img

        playerBulletImpact = map (\x -> f x 11 12 12) [1, 15 .. 29]

loadEnemyCharacterSheet :: FilePath -> IO EnemyCharacterSheet
loadEnemyCharacterSheet f = do
  x <- readImage f
  case x of
    Left err -> error err
    Right dynImg ->
      return $
        EnemyCharacterSheet
          crabIdle
          crabWalkLeft
          crabWalkRight
      where
        img = convertRGBA8 dynImg
        f :: Int -> Int -> Int -> Int -> Picture
        f x y w h = fromImageRGBA8 $ crop x y w h img

        crabIdle = f 1 31 16 14
        crabWalkLeft = map (\x -> f x 31 16 14) [19, 37 .. 55]
        crabWalkRight = map (\x -> f x 31 16 14) [73, 91 .. 109]

getImageAsset :: Assets -> String -> Picture
getImageAsset a s = case lookup s (images a) of
  Just p -> p
  Nothing -> error $ "Image asset '" ++ s ++ "' does not exist."

-- loadCharacterSheets :: FilePath -> IO CharacterSheets
-- loadCharacterSheet f = do
--   img <- loadJuicyPNG f
