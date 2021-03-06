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
  fxCharacterSheet <- loadFxSheet (f </> "FxSheet.png")
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

loadFxSheet :: FilePath -> IO FxSheet
loadFxSheet f = do
  x <- readImage f
  case x of
    Left err -> error err
    Right dynImg ->
      return $
        FxSheet
          playerBullets
          greenBulletImpact
          redBulletImpact
          blueBulletImpact
          playerDeath
          enemyBullet
          smallExplosions
          explosions
          fireball
      where
        img = convertRGBA8 dynImg
        f :: Int -> Int -> Int -> Int -> Picture
        f x y w h = fromImageRGBA8 $ crop x y w h img

        playerBullets = [f 2 2 6 6, f 12 2 6 6, f 23 3 4 4]
        greenBulletImpact = map (\x -> f x 11 12 12) [1, 15 .. 29]
        redBulletImpact = map (\x -> f x 35 12 12) [15, 29 .. 29]
        blueBulletImpact = map (\x -> f x 11 12 12) [43, 57 .. 71]
        playerDeath = map (\x -> f x 237 48 48) [1, 51 .. 301]
        enemyBullet = f 11 25 8 8
        smallExplosions = map (\x -> f x 49 16 16) [1, 19 .. 127]
        explosions = map (\x -> f x 91 30 30) [1, 33 .. 225]
        fireball = map (\x -> f x 389 22 22) [1, 25 .. 361]

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
          sunIdle
          sunShooting
      where
        img = convertRGBA8 dynImg
        f :: Int -> Int -> Int -> Int -> Picture
        f x y w h = fromImageRGBA8 $ crop x y w h img

        crabIdle = f 1 31 16 14
        crabWalkLeft = map (\x -> f x 31 16 14) [19, 37 .. 55]
        crabWalkRight = map (\x -> f x 31 16 14) [73, 91 .. 109]
        sunIdle = map (\x -> f x 87 16 16) [1, 19 .. 73]
        sunShooting = f 91 87 16 16

getImageAsset :: Assets -> String -> Picture
getImageAsset a s = case lookup s (images a) of
  Just p -> p
  Nothing -> error $ "Image asset '" ++ s ++ "' does not exist."

-- loadCharacterSheets :: FilePath -> IO CharacterSheets
-- loadCharacterSheet f = do
--   img <- loadJuicyPNG f
