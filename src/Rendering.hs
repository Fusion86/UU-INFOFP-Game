module Rendering where

import Assets
import Colors
import Common
import Coordinates
import Coordinates (menuHeight)
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
    translate,
    violet,
  )
import Graphics.Gloss.SDL.Surface (CacheTexture (..), bitmapOfSurface, withSdlSurface)
import Levels
import Model
import SDL.Font (Font, solid)
import SDL.Vect (V4 (..))
import Prelude hiding (lookup)

renderString :: Font -> Color -> String -> IO Picture
renderString = renderString' OriginTopLeft

renderCenterString :: Font -> Color -> String -> IO Picture
renderCenterString = renderString' OriginCenter

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
  -- TODO: Maybe use getScreenSize to automatically determine the viewScale?
  -- This also requires changes to the viewWidth and viewHeight functions.
  return $ scale viewScale viewScale world

renderWorld :: Assets -> Font -> TileSet -> [Level] -> World -> IO Picture
renderWorld a f _ _ (World IntroScene {} _) = return $ getImageAsset a "Intro"
renderWorld a f _ _ w@(World (MenuScene MainMenu _ selectedItem) _) = do
  -- TODO: Maybe use caching?
  gameTxt <- renderCenterString f red "Game"
  subTxt <- renderCenterString f white "UU-INFOFP"
  menuTxts <- renderMenuItems f selectedItem ["Start", "Level Select", "Quit"]
  return $
    pictures
      [ getImageAsset a "MainMenuBg",
        setPos (240, 120) $ scale 4 4 gameTxt,
        setPos (248, 96) subTxt,
        renderList (240, 188) 12 menuTxts
      ]
renderWorld a f t l w@(World (MenuScene LevelSelectMenu _ selectedItem) _) = do
  selectLevelTxt <- renderCenterString f white "Select a level"
  levelTxts <- renderMenuItems f selectedItem (map levelName l)
  let (bg, fg) = renderLevel 0 a t selectedLevel
  hud <- renderHud 0 a f initPlayer
  return $
    pictures
      [ bg,
        fg,
        hud,
        setPos (240, 44) selectLevelTxt,
        renderList (240, 88) 12 levelTxts
      ]
  where
    selectedLevel = l !! selectedItem
renderWorld a f t _ w@(World (Gameplay levelInstance p pt) _) = do
  let (bg, fg) = renderLevel pt a t (level levelInstance)
  hud <- renderHud pt a f p
  return $
    pictures
      [ bg,
        renderEntities a (levelEntities levelInstance),
        renderEnemies pt a (levelEnemies levelInstance),
        -- render pickups
        -- render enemies
        renderPlayer pt a w p,
        fg,
        hud,
        renderCursor a w
      ]
renderWorld _ f _ _ (World (MenuScene PauseMenu _ selectedItem) _) = do
  pausedTxt <- renderCenterString f white "Game Paused"
  menuTxts <- renderMenuItems f selectedItem ["Resume", "Quit"]
  return $
    pictures
      [ setPos (240, 96) pausedTxt,
        renderList (240, 188) 12 menuTxts
      ]
renderWorld _ f _ _ _ = do
  str <- renderString f red "Scene not implemented"
  return $ setPos (240, 160) str

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
      | currentIndex == selectedIndex = renderCenterString font red x : rest
      -- Otherwise just render it the default color (white for now).
      | otherwise = renderCenterString font white x : rest
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
    -- Hacky offset because our pre-rendered images do not contain empty space for the menu.
    offset = translate 0 (menuHeight / 2)

    renderLayer :: TileLayer -> [Picture]
    renderLayer = renderTileGrid ft tileSet . tileGrid

    -- TODO: Parallax scroll background image based on player position
    bgLayers = filter ((==) BackgroundTileLayer . tileLayerType) layers
    renderedBackground
      | Just bgImage <- background = pictures $ offset (getImageAsset a bgImage) : rest
      | otherwise = pictures rest
      where
        rest = concatMap renderLayer bgLayers

    fgLayers = filter ((==) ForegroundTileLayer . tileLayerType) layers
    renderedForeground
      | Just fgImage <- foreground = pictures $ offset (getImageAsset a fgImage) : rest
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
    t = round ft

renderCursor :: Assets -> World -> Picture
renderCursor a (World _ (Input _ _ p)) = setPos p $ getImageAsset a "Cursor"

renderPlayer :: Float -> Assets -> World -> Player -> Picture
renderPlayer ft a w p =
  setPos (x, y) pic
  where
    (mx, my) = pointer $ input w
    (x, y) = playerPosition p
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
    renderEntity :: LevelEntity -> Picture
    renderEntity x = setPos (entityPosition x) $ getEntityPicture (entityType x)

    getEntityPicture :: EntityType -> Picture
    getEntityPicture (Bullet _) = getImageAsset a "BulletTemp"
    getEntityPicture (ExplosionEntity totalLifetime lifetime) = playerBulletImpact (fxSheet a) !! frame
      where
        -- frame = min 2 $ dbg "test" $ floor $ (1 - (totalLifetime - lifetime) / totalLifetime) * 3 - 1
        frame = min 2 $ floor $ (1 - (totalLifetime - lifetime) / totalLifetime) * 3
    getEntityPicture _ = renderDbgString red "entity not implemented"

renderEnemies :: Float -> Assets -> [EnemyInstance] -> Picture
renderEnemies ft a = pictures . map renderEnemy
  where
    sheet = enemyCharacterSheet a

    renderEnemy :: EnemyInstance -> Picture
    renderEnemy x = setPos (enemyPosition x) $ getEnemyPicture (enemyType x)
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
  -- TODO: Use map for this
  hpTxt <- renderString f white "HP: 100/100"
  wpn1Txt <- renderString f (getColor AssaultRifle) "1. Rifle"
  wpn2Txt <- renderString f (getColor PeaShooter) "2. Peanuts"
  wpn3Txt <- renderString f (getColor Shotgun) "3. Shotgun"
  wpn4Txt <- renderString f (getColor RocketLauncher) "4. Rockets"
  return $
    pictures
      [ getImageAsset a "HUDFrame",
        -- Text rendering is not pixel perfect, so these numbers differ a bit from the PSD=
        setPos (9, 318) hpTxt,
        setPos (8, 333) wpn1Txt,
        setPos (9, 348) wpn2Txt,
        setPos (161, 333) wpn3Txt,
        setPos (161, 348) wpn4Txt
      ]
  where
    selectedWeapon = playerSelectedWeapon pl

    getColor wpn
      | wpn == selectedWeapon = red
      | otherwise = white
