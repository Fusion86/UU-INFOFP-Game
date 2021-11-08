module Rendering where

import Assets
import Colors
import Common
import Coordinates
import Data.Map (lookup)
import Data.Text (pack)
import Data.Word (Word8)
import Graphics.Gloss
  ( Color,
    Picture,
    blank,
    circleSolid,
    color,
    green,
    line,
    pictures,
    rectangleWire,
    rgbaOfColor,
    scale,
    translate,
  )
import Graphics.Gloss.Interface.IO.Interact (Key (SpecialKey), SpecialKey (KeyF1))
import Graphics.Gloss.SDL.Surface (CacheTexture (..), bitmapOfSurface, withSdlSurface)
import Input
import Levels
import Model
import SDL.Font (Font, solid)
import SDL.Vect (V4 (..))
import Text.Printf (printf)
import Prelude hiding (lookup)

renderString :: Font -> Color -> String -> IO Picture
renderString = renderString' OriginTopLeft

renderStringCenter :: Font -> Color -> String -> IO Picture
renderStringCenter = renderString' OriginCenter

-- | Render a string with given font and color. The origin is the middle of the string.
-- Does not cache, and maybe it leaks memory idk.
renderString' :: OriginPoint -> Font -> Color -> String -> IO Picture
renderString' o f c str = do
  surface <- solid f (colorCvt c) (pack str)
  ((w, h), txt) <- bitmapOfSurface NoCache surface
  let pic = case o of
        OriginTopLeft -> translate (w / 2) (h / (-2)) txt
        OriginCenter -> txt
  return pic
  where
    colorCvt :: Color -> V4 Word8
    colorCvt c =
      let (r, g, b, a) = rgbaOfColor c
       in V4 (floor $ r * 255) (floor $ g * 255) (floor $ b * 255) (floor $ a * 255)

renderWorldScaled :: Assets -> Font -> TileSet -> [Level] -> World -> IO Picture
renderWorldScaled a f t l w = do
  world <- renderWorld a f t l w
  let s = viewScale (input w)
  return $ scale s s world

renderWorld :: Assets -> Font -> TileSet -> [Level] -> World -> IO Picture
renderWorld a f _ _ (World IntroScene {} _) = return $ getImageAsset a "Intro"
renderWorld a f _ _ w@(World (MenuScene MainMenu _ selectedItem) _) = do
  gameTxt <- renderStringCenter f red "Game"
  subTxt <- renderStringCenter f white "UU-INFOFP"
  menuTxts <- renderMenuItems f selectedItem ["Start", "Level Select", "Quit"]
  return $
    pictures
      [ getImageAsset a "MainMenuBg",
        setPos (288, 120) $ scale 4 4 gameTxt,
        setPos (296, 96) subTxt,
        renderList (288, 188) 12 menuTxts
      ]
renderWorld a f t l w@(World (MenuScene LevelSelectMenu _ selectedItem) _) = do
  selectLevelTxt <- renderStringCenter f white "Select a level"
  levelTxts <- renderMenuItems f selectedItem (map levelName l)
  let (bg, fg) = renderLevel 0 a t selectedLevel
  return $
    pictures
      [ bg,
        fg,
        setPos (288, 44) selectLevelTxt,
        renderList (288, 88) 12 levelTxts
      ]
  where
    selectedLevel = l !! selectedItem
renderWorld a f t _ w@(World (Gameplay levelInstance pl pt) i) = do
  let (bg, fg) = renderLevel pt a t (level levelInstance)
  hud <- renderHud pt a f pl
  return $
    pictures
      [ bg,
        renderEnemies pt a (levelEnemies levelInstance),
        renderEntities a (levelEntities levelInstance),
        renderPlayer pt a w pl,
        fg,
        hud,
        renderCursor a w,
        debugOverlay
      ]
  where
    debugOverlay
      | debugMode i =
        pictures $
          [renderDebugOverlay w, renderObjDebugOverlay pl]
            ++ map renderObjDebugOverlay (levelEntities levelInstance)
            ++ map renderObjDebugOverlay (levelEnemies levelInstance)
            ++ map renderObjDebugOverlay (levelObjects (level levelInstance))
      | otherwise = blank
renderWorld _ f _ _ (World (MenuScene PauseMenu _ selectedItem) _) = do
  pausedTxt <- renderStringCenter f white "Game Paused"
  menuTxts <- renderMenuItems f selectedItem ["Resume", "Quit"]
  return $
    pictures
      [ setPos (288, 96) pausedTxt,
        renderList (288, 188) 12 menuTxts
      ]
renderWorld a f t levels w@(World (Benchmark benchWorld rt) i) = do
  benchWorld <- renderWorld a f t levels benchWorld
  txts <-
    mapM
      (renderString f white)
      [ "Benchmark",
        "render scale: " ++ show (viewScale i),
        "remaining time: " ++ printf "%0.2f" rt
      ]
  return $
    pictures
      [ benchWorld,
        renderList (8, 8) 12 txts
      ]
renderWorld _ f _ _ _ = do
  str <- renderStringCenter f red "Scene not implemented"
  return $ setPos (288, 160) str

renderMenuItems :: Font -> Int -> [String] -> IO [Picture]
renderMenuItems font selectedIndex xs = sequence (helper 0 xs)
  where
    -- TODO: Cleanup spaghetti code
    -- Returns the index of each item that is visible in the menu.
    visibleItemRange = take visibleItemCount (drop dropCount [0 .. (length xs - 1)])
      where
        -- Number of visible items in the menu.
        visibleItemCount = 9
        -- The middle item, zero indexed
        midPoint = visibleItemCount `div` 2
        -- max items we can drop without showing empty slots at the end
        maxDropCount = max 0 (length xs - visibleItemCount)
        -- the amount of items we want to drop to center the selected one
        initialDropCount = selectedIndex - midPoint
        -- the end result
        dropCount
          | selectedIndex > midPoint = min maxDropCount initialDropCount
          | otherwise = 0

    helper :: Int -> [String] -> [IO Picture]
    helper _ [] = []
    helper currentIndex (x : xs)
      -- Skip items that aren't in the visible item range (scroll view).
      | currentIndex `notElem` visibleItemRange = rest
      -- If the item is the selected item highlight it red.
      | currentIndex == selectedIndex = renderStringCenter font red x : rest
      -- Otherwise just render it the default color (white for now).
      | otherwise = renderStringCenter font white x : rest
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

renderLevel :: Float -> Assets -> TileSet -> Level -> (Picture, Picture)
renderLevel ft a tileSet (Level name background foreground layers objects) =
  (renderedBackground, renderedForeground)
  where
    renderLayer :: TileLayer -> [Picture]
    renderLayer = renderTileGrid ft tileSet . tileGrid

    -- TODO: Parallax scroll background image based on player position
    bgLayers = filter ((==) BackgroundTileLayer . tileLayerType) layers
    renderedBackground
      | Just bgImage <- background = pictures $ getImageAsset a bgImage : rest
      | otherwise = pictures rest
      where
        rest = concatMap renderLayer bgLayers

    fgLayers = filter ((==) ForegroundTileLayer . tileLayerType) layers
    renderedForeground
      | Just fgImage <- foreground = pictures $ getImageAsset a fgImage : rest
      | otherwise = pictures rest
      where
        rest = concatMap renderLayer fgLayers

renderTileGrid :: Float -> TileSet -> TileGrid -> [Picture]
renderTileGrid ft _ [] = []
renderTileGrid ft ts (((x, y), tile) : is)
  | Just pic <- renderTile ft ts tile (x + 4, y + 4) = pic : renderTileGrid ft ts is
  -- If tile can't be rendered (should never happen though)
  | otherwise = trace ("Can't render tile: " ++ show tile) renderTileGrid ft ts is

renderTile :: Float -> TileSet -> Int -> Vec2 -> Maybe Picture
renderTile _ ts 0 xy = Nothing
renderTile ft ts i xy
  | Just p <- lookup (animateTile ft i) ts = Just $ setPos xy p
  | otherwise = Just $ renderDbgString red ("!tile: " ++ show i)

-- TODO: Rewrite this to use random animations, instead of time based.
animateTile :: Float -> Int -> Int
animateTile ft i
  -- Acid
  | i == 673 && odd t = 674
  | i == 674 && odd t = 673
  -- Laser
  | i == 457 && odd lt = 459
  | i == 458 && odd lt = 460
  | i == 501 && odd lt = 503
  | i == 502 && odd lt = 504
  | i == 545 && odd lt = 547
  | i == 546 && odd lt = 548
  | i == 589 && odd lt = 591
  | i == 590 && odd lt = 592
  -- Non animated tile
  | otherwise = i
  where
    t = round ft

    -- Laser t, magic numbers.
    lt = floor ((ft - fromIntegral (floor ft)) * 3) `mod` 2

renderCursor :: Assets -> World -> Picture
renderCursor a (World _ i) = setPos (pointer i) $ getImageAsset a "Cursor"

renderPlayer :: Float -> Assets -> World -> Player -> Picture
renderPlayer ft a w p =
  -- Small offset because the player bounding box is not the same size as the texture.
  setPos (x, y - 4) pic
  where
    (mx, my) = pointer $ input w
    (x, y) = center p
    -- Direction of the mouse pointer relative to the player.
    (dx, dy) = (mx - x, my - y)
    sheet = playerSheet a
    isIdle = playerState p == IdleState
    -- Which picture/sprite to display
    pic
      -- Idle states
      | isIdle && dx > 0 = playerIdleRight sheet
      | isIdle = playerIdleLeft sheet
      | dx > 0 = playerWalkRight sheet !! frame
      | otherwise = playerWalkLeft sheet !! frame
      where
        -- Returns 0-7 each second (inclusive), giving a framerate of 8 FPS.
        frame :: Int
        frame = floor $ (ft - fromIntegral (floor ft)) * 8

renderEntities :: Assets -> [LevelEntity] -> Picture
renderEntities a = pictures . map renderEntity
  where
    fx = fxSheet a

    renderEntity :: LevelEntity -> Picture
    renderEntity x = setPos (center x) $ getEntityPicture (entityType x)

    getEntityPicture :: EntityType -> Picture
    getEntityPicture Bullet {bulletType = PeaShooter} = playerBullets fx !! 2
    getEntityPicture Bullet {} = playerBullets fx !! 0
    getEntityPicture (ExplosionEntity t totalLifetime lifetime) = frame
      where
        frame
          | t == DamageExplosion = explosions (fxSheet a) !! getFrame 8
          | otherwise = playerBulletImpact (fxSheet a) !! getFrame 3

        getFrame :: Int -> Int
        getFrame i = min (i - 1) $ floor $ (1 - (totalLifetime - lifetime) / totalLifetime) * fromIntegral i
    getEntityPicture _ = renderDbgString red "entity not implemented"

renderEnemies :: Float -> Assets -> [EnemyInstance] -> Picture
renderEnemies ft a = pictures . map renderEnemy
  where
    sheet = enemyCharacterSheet a

    renderEnemy :: EnemyInstance -> Picture
    renderEnemy x = setPos (center x) $ getEnemyPicture (enemyType x)
      where
        (vx, vy) = enemyVelocity x

        getEnemyPicture :: EnemyType -> Picture
        getEnemyPicture CrabEnemy
          | vx > 0 = crabWalkRight sheet !! frame
          | vx < 0 = crabWalkLeft sheet !! frame
          | otherwise = crabIdle sheet
          where
            frame :: Int
            frame = floor ((ft - fromIntegral (floor ft)) * 6) `mod` 3 -- Length of crabWalkRight and crabWalkLeft
        getEnemyPicture _ = renderDbgString red "entity not implemented"

renderHud :: Float -> Assets -> Font -> Player -> IO Picture
renderHud pt a f pl = do
  hpTxt <- renderString f white "HP: 100/100"
  wpn1Txt <- renderString f (getColor AssaultRifle) "1. Rifle"
  wpn2Txt <- renderString f (getColor PeaShooter) "2. Peanuts"
  wpn3Txt <- renderString f (getColor SniperRifle) "3. Sniper"
  wpn4Txt <- renderString f (getColor RocketLauncher) "4. Rockets"
  return $
    pictures
      [ getImageAsset a "HUDFrame",
        -- Text rendering is not pixel perfect, so these numbers differ a bit from the PSD=
        setPos (9, 292) hpTxt,
        setPos (8, 307) wpn1Txt,
        setPos (9, 322) wpn2Txt,
        setPos (161, 307) wpn3Txt,
        setPos (161, 322) wpn4Txt
      ]
  where
    selectedWeapon = playerSelectedWeapon pl

    getColor wpn
      | wpn == selectedWeapon = red
      | otherwise = white

renderDebugOverlay :: World -> Picture
renderDebugOverlay (World s i) =
  setPos (10, 10) $ renderDbgString green $ "timeMultiplier: " ++ show (timeMultiplier i) ++ extra s
  where
    extra (Gameplay _ _ pt) = " playTime: " ++ printf "%0.2f" pt
    extra _ = ""

renderObjDebugOverlay :: Object2D a => a -> Picture
renderObjDebugOverlay o = pictures [boundingBox, origin, namePic]
  where
    (w, h) = size o

    namePic = setPos (position o) $ renderDbgString green $ name o
    origin = setPos (position o) $ color green $ circleSolid 1
    boundingBox = setPos (center o) $ color red $ rectangleWire w h
