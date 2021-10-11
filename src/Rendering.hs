module Rendering where

import Assets
import Common
import Coordinates
import Data.Map (lookup)
import Data.Text (pack)
import Data.Word (Word8)
import Graphics.Gloss
import Graphics.Gloss.SDL.Surface (CacheTexture (..), bitmapOfSurface, withSdlSurface)
import Levels
import Model
import SDL.Font (Font, solid)
import SDL.Vect (V4 (..))
import Prelude hiding (lookup)

-- | Render a string with given font and color. The origin is the middle of the string.
-- Does not cache, and maybe it leaks memory idk.
renderString :: Font -> Color -> String -> IO Picture
renderString f c str = do
  surface <- solid f (colorCvt c) (pack str)
  ((dw, dh), bg) <- bitmapOfSurface NoCache surface
  return bg
  where
    colorCvt :: Color -> V4 Word8
    colorCvt c =
      let (r, g, b, a) = rgbaOfColor c
       in V4 (floor $ r * 255) (floor $ g * 255) (floor $ b * 255) (floor $ a * 255)

renderWorldScaled :: Assets -> Font -> TileSet -> [Level] -> World -> IO Picture
renderWorldScaled a f t l w = do
  world <- renderWorld a f t l w
  return $ scale worldScale worldScale world

renderWorld :: Assets -> Font -> TileSet -> [Level] -> World -> IO Picture
renderWorld a f _ _ (World IntroScene {} _ pl) = return $ getAsset a "Intro"
renderWorld a f _ _ w@(World (MenuScene MainMenu _ selectedItem) _ pl) = do
  -- TODO: Maybe use caching?
  gameTxt <- renderString f red "Game"
  subTxt <- renderString f white "UU-INFOFP"
  menuTxts <- renderMenuItems f selectedItem ["Start", "Level Select", "Quit"]
  return $
    pictures
      [ getAsset a "MainMenuBg",
        setPos (120, 60) $ scale 4 4 gameTxt,
        setPos (128, 48) subTxt,
        renderList (120, 94) 12 menuTxts,
        renderCursor a w
      ]
  where
    getColor :: Int -> Color
    getColor itemIdx
      | selectedItem == itemIdx = red
      | otherwise = violet
renderWorld a f t l w@(World (MenuScene LevelSelectMenu _ selectedItem) _ pl) = do
  selectLevelTxt <- renderString f white "Select a level"
  levelTxts <- renderMenuItems f selectedItem (map levelName l)
  return $
    pictures
      [ renderLevel a t selectedLevel,
        setPos (120, 24) selectLevelTxt,
        renderList (120, 44) 12 levelTxts,
        renderCursor a w
      ]
  where
    selectedLevel = l !! selectedItem
renderWorld _ f _ _ _ = do
  str <- renderString f red "Scene not implemented"
  return $ setPos (120, 80) str

renderMenuItems :: Font -> Int -> [String] -> IO [Picture]
renderMenuItems font selectedIndex xs = sequence (helper 0 xs)
  where
    -- TODO: Cleanup spaghetti code
    -- Returns the index of each item that is visible in the menu.
    visibleItemRange =
      let range = take visibleItemCount (drop dropCount [0 .. (length xs - 1)])
       in range
      where
        -- Number of visible items in the menu.
        visibleItemCount = 9

        dropCount
          | selectedIndex >= length xs - visibleItemCount = min (selectedIndex - visibleItemCount `div` 2) 8
          | otherwise = max 0 (selectedIndex - visibleItemCount `div` 2)

    helper :: Int -> [String] -> [IO Picture]
    helper _ [] = []
    helper currentIndex (x : xs)
      -- Skip items that aren't in the visible item range (scroll view).
      | currentIndex `notElem` visibleItemRange = rest
      -- If the item is the selected item highlight it red.
      | currentIndex == selectedIndex = renderString font red x : rest
      -- Otherwise just render it the default color (white for now).
      | otherwise = renderString font white x : rest
      where
        rest = helper (currentIndex + 1) xs

-- | Render a horizontal list starting at `start`.
-- Each next element will be `spacing` px lower than the element before it.
renderList :: (Float, Float) -> Float -> [Picture] -> Picture
renderList start spacing = pictures . helper start
  where
    helper :: (Float, Float) -> [Picture] -> [Picture]
    helper _ [] = []
    helper (x, y) (p : ps) = setPos (x, y) p : helper (x, y + spacing) ps

renderLevel :: Assets -> TileSet -> Level -> Picture
renderLevel a tileSet (Level name background layers objects)
  | Just invalidLayer <- validateLayers layers =
    let tiles = layers !! invalidLayer
     in pictures
          [ setPos (10, 20) $ renderDbgString red $ "renderLevel: InvalidTileCount on layer " ++ show invalidLayer,
            setPos (10, 34) $ renderDbgString red $ "expected: " ++ show expectedTileCount,
            setPos (10, 48) $ renderDbgString red $ "actual: " ++ show (length tiles)
          ]
  -- TODO: Parallax scroll background based on player position
  | otherwise = pictures $ getAsset a background : concatMap (helper (4, 4)) layers
  where
    expectedTileCount = round (worldWidth / 8 * worldHeight / 8)

    helper :: (Float, Float) -> [Int] -> [Picture]
    helper _ [] = []
    helper (x, y) lst@(tile : tiles)
      -- If end of row, go to next row.
      | x >= worldWidth = helper (4, y + 8) lst
      | otherwise = renderTile tileSet tile (x, y) : helper (x + 8, y) tiles

renderTile :: TileSet -> Int -> (Float, Float) -> Picture
renderTile t 0 xy = setPos xy blank
renderTile t i xy
  | Just p <- lookup i t = setPos xy p
  | otherwise = renderDbgString red ("!tile: " ++ show i)

renderCursor :: Assets -> World -> Picture
renderCursor a (World _ (Input _ _ p) _) = setPos p $ getAsset a "Cursor"
