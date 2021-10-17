module Rendering where

import Assets
import Colors
import Common
import Coordinates
import Data.Cache (fetchWithCache)
import Data.Map (lookup)
import Data.Text (pack)
import Data.Word (Word8)
import Graphics.Gloss
  ( Color,
    Picture,
    blank,
    blue,
    color,
    pictures,
    rectangleSolid,
    rgbaOfColor,
    scale,
    violet,
  )
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

renderWorldScaled :: RenderCache -> Assets -> Font -> TileSet -> [Level] -> World -> IO Picture
renderWorldScaled rc a f t l w = do
  world <- renderWorld rc a f t l w
  return $ scale worldScale worldScale world

renderWorld :: RenderCache -> Assets -> Font -> TileSet -> [Level] -> World -> IO Picture
renderWorld _ a f _ _ (World IntroScene {} _) = return $ getAsset a "Intro"
renderWorld _ a f _ _ w@(World (MenuScene MainMenu _ selectedItem) _) = do
  -- TODO: Maybe use caching?
  gameTxt <- renderString f red "Game"
  subTxt <- renderString f white "UU-INFOFP"
  menuTxts <- renderMenuItems f selectedItem ["Start", "Level Select", "Quit"]
  return $
    pictures
      [ getAsset a "MainMenuBg",
        setPos (240, 120) $ scale 4 4 gameTxt,
        setPos (248, 96) subTxt,
        renderList (240, 188) 12 menuTxts
      ]
renderWorld rc a f t l w@(World (MenuScene LevelSelectMenu _ selectedItem) _) = do
  selectLevelTxt <- renderString f white "Select a level"
  levelTxts <- renderMenuItems f selectedItem (map levelName l)
  (bg, mg, fg) <- renderLevel rc 0 a t selectedLevel
  return $
    pictures
      [ bg,
        mg,
        fg,
        setPos (240, 44) selectLevelTxt,
        renderList (240, 88) 12 levelTxts
      ]
  where
    selectedLevel = l !! selectedItem
renderWorld rc a f t _ w@(World (Gameplay levelInstance p pt) _) = do
  (bg, mg, fg) <- renderLevel rc pt a t (level levelInstance)
  return $
    pictures
      [ bg,
        mg,
        -- render pickups
        -- render enemies
        renderPlayer a p,
        fg,
        renderCursor a w
      ]
renderWorld _ _ f _ _ (World (MenuScene PauseMenu _ selectedItem) _) = do
  pausedTxt <- renderString f white "Game Paused"
  menuTxts <- renderMenuItems f selectedItem ["Resume", "Quit"]
  return $
    pictures
      [ setPos (240, 96) pausedTxt,
        renderList (240, 188) 12 menuTxts
      ]
renderWorld _ _ f _ _ _ = do
  str <- renderString f red "Scene not implemented"
  return $ setPos (240, 160) str

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
renderList :: Vec2 -> Float -> [Picture] -> Picture
renderList start spacing = pictures . helper start
  where
    helper :: Vec2 -> [Picture] -> [Picture]
    helper _ [] = []
    helper (x, y) (p : ps) = setPos (x, y) p : helper (x, y + spacing) ps

renderLevel :: RenderCache -> Float -> Assets -> TileSet -> Level -> IO (Picture, Picture, Picture)
renderLevel rc t a tileSet (Level name background layers objects) = do
  bg <- cachedBackground
  sl <- cachedSolids
  fg <- cachedForeground
  return (bg, sl, fg)
  where
    cacheKey = name
    cachedBackground = fetchWithCache rc (cacheKey ++ "_bg") (const renderedBackground)
    cachedSolids = fetchWithCache rc (cacheKey ++ "_sl") (const renderedSolidLayers)
    cachedForeground = fetchWithCache rc (cacheKey ++ "_fg") (const renderedForeground)

    -- TODO: Parallax scroll background image based on player position
    bgLayers = filter ((==) BackgroundTileLayer . tileLayerType) layers
    renderedBackground
      | Just bgImage <- background = return $ pictures $ getAsset a bgImage : rest
      | otherwise = return $ trace "renderBg" pictures rest
      where
        rest = concatMap renderLayer bgLayers

    solidLayers = safeHead $ filter ((==) SolidTileLayer . tileLayerType) layers
    renderedSolidLayers = return $ pictures $ concatMap renderLayer solidLayers

    fgLayers = filter ((==) ForegroundTileLayer . tileLayerType) layers
    renderedForeground = return $ pictures $ concatMap renderLayer fgLayers

    renderLayer = renderTileGrid t tileSet . tileGrid

renderTileGrid :: Float -> TileSet -> TileGrid -> [Picture]
renderTileGrid t _ [] = []
renderTileGrid t ts (((x, y), tile) : is)
  | Just pic <- renderTile t ts tile (x + 4, y + 4) = pic : renderTileGrid t ts is
  -- If tile can't be rendered (should never happen though)
  | otherwise = trace ("Can't render tile: " ++ show tile) renderTileGrid t ts is

renderTile :: Float -> TileSet -> Int -> Vec2 -> Maybe Picture
renderTile _ ts 0 xy = Nothing
renderTile t ts i xy
  | Just p <- lookup (animateTile t i) ts = Just $ setPos xy p
  | otherwise = Just $ renderDbgString red ("!tile: " ++ show i)

-- TODO: Rewrite this to use random animations, instead of time based.
animateTile :: Float -> Int -> Int
animateTile time i
  -- Acid
  | i == 673 && odd t = 674
  | i == 674 && odd t = 673
  -- Laser
  | i == 457 && odd t = 459
  | i == 458 && odd t = 460
  | i == 501 && odd t = 503
  | i == 502 && odd t = 504
  | i == 545 && odd t = 547
  | i == 546 && odd t = 548
  | i == 589 && odd t = 591
  | i == 590 && odd t = 592
  -- Non animated tile
  | otherwise = i
  where
    t = round time

renderCursor :: Assets -> World -> Picture
renderCursor a (World _ (Input _ _ p)) = setPos p $ getAsset a "Cursor"

renderPlayer :: Assets -> Player -> Picture
renderPlayer a (Player _ _ _ _ _ _ (x, y)) = setPos (x, y) $ color blue $ rectangleSolid 34 24
