module Rendering where

import Assets
import Collision
import Colors
import Common
import Control.Monad.Extra (concatMapM)
import Control.Monad.Random (Rand, RandomGen, evalRand, getRandomR, mkStdGen)
import qualified Control.Monad.Random as Rng (fromList, uniform)
import Coordinates
import Data.Functor ((<&>))
import Data.Map (lookup)
import Data.Maybe (isJust)
import Data.Text (pack)
import Data.Word (Word8)
import Graphics.Gloss
  ( Color,
    Picture (Bitmap),
    bitmapSize,
    black,
    blank,
    circleSolid,
    color,
    green,
    line,
    pictures,
    polygon,
    rectangleWire,
    rgbaOfColor,
    scale,
    translate,
  )
import Graphics.Gloss.Interface.IO.Interact (Key (SpecialKey), SpecialKey (KeyF1))
import Graphics.Gloss.SDL.Surface (CacheTexture (..), bitmapOfSurface, withSdlSurface)
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
renderWorldScaled a f t l world@World {input = i} = do
  world <- renderWorld a f t l world
  let s = viewScale i
  return
    ( if debugMode i
        then pictures [scale s s world]
        else pictures [scale s s world, frameLeft, frameRight, frameTop, frameBottom]
    )
  where
    (w, h) = (viewWidth i, viewHeight i)
    (gw, gh) = (gameWidth * viewScale i, gameHeight * viewScale i)
    topLeft = (- (w / 2), - (h / 2))
    topRight = (w / 2, - (h / 2))
    bottomLeft = (- (w / 2), h / 2)
    bottomRight = (w / 2, h / 2)
    left = - (gw / 2)
    right = gw / 2
    top = - (gh / 2)
    bottom = gh / 2
    leftV = - (w / 2)
    rightV = w / 2
    topV = - (h / 2)
    bottomV = h / 2

    frameLeft = color black $ polygon [topLeft, (left, topV), (left, bottomV), bottomLeft]
    frameRight = color black $ polygon [(right, topV), topRight, bottomRight, (right, bottomV)]
    frameTop = color black $ polygon [topLeft, topRight, (rightV, top), (leftV, top)]
    frameBottom = color black $ polygon [(leftV, bottom), (rightV, bottom), bottomRight, bottomLeft]

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
        setPos (296, 106) subTxt,
        renderList (288, 188) 12 menuTxts
      ]
renderWorld a f t l w@(World (MenuScene LevelSelectMenu _ selectedItem) _) = do
  selectLevelTxt <- renderStringCenter f white "Select a level"
  levelTxts <- renderMenuItems f selectedItem (map levelName l)
  let (bg, fg) = renderLevel 0 a t selectedLevel (gameWidth / 2, gameHeight / 2)
  return $
    pictures
      [ bg,
        fg,
        setPos (288, 44) selectLevelTxt,
        renderList (288, 88) 12 levelTxts
      ]
  where
    selectedLevel = l !! selectedItem
renderWorld a f t _ w@(World (Gameplay gp@GameplayScene {player = pl}) i) = do
  let (bg, fg) = renderLevel pt a t (level lvlInst) (playerPosition pl)
  hud <- renderHud pt a f pl
  return $
    pictures
      [ bg,
        renderEnemies pt a (levelEnemies lvlInst),
        renderPlayer pt a w pl,
        fg,
        renderEntities pt a (levelEntities lvlInst),
        hud,
        renderCursor a w,
        debugOverlay
      ]
  where
    lvlInst = levelInstance gp
    pt = playTime gp
    pl = player gp

    debugOverlay
      | debugMode i =
        pictures $
          [renderDebugOverlay w, renderObjDebugOverlay pl]
            ++ map renderObjDebugOverlay (levelEntities lvlInst)
            ++ map renderObjDebugOverlay (levelEnemies lvlInst)
            ++ map renderObjDebugOverlay (levelObjects (level lvlInst))
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
renderWorld a f _ _ w@(World (MenuScene (EndOfLevel gp nextLevel score) _ selectedItem) _) = do
  endTxt <- renderStringCenter f white endTextStr
  surviveTxt <- renderStringCenter f gray ("You survived for " ++ printf "%0.0f" (playTime gp) ++ " seconds")
  scoreTxt <- renderStringCenter f gray ("During this time you killed " ++ show score ++ " enemies")
  menuTxts <- renderMenuItems f selectedItem menuItems
  return $
    pictures
      [ getImageAsset a "EndOfLevelBg",
        setPos (288, 96) endTxt,
        setPos (288, 108) surviveTxt,
        setPos (288, 120) scoreTxt,
        renderList (288, 188) 12 menuTxts
      ]
  where
    endTextStr
      | isJust nextLevel = "Level Completed"
      | otherwise = "You Died"

    menuItems
      | isJust nextLevel = ["Next Level", "Quit"]
      | otherwise = ["Quit"]

-- renderWorld _ f _ _ _ = do
--   str <- renderStringCenter f red "Scene not implemented"
--   return $ setPos (gameWidth / 2, gameHeight / 2) str

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

renderLevel :: Float -> Assets -> TileSet -> Level -> Vec2 -> (Picture, Picture)
renderLevel pt a tileSet (Level name background foreground parallax layers objects) pos@(x, y) =
  evalRand
    ( do
        bg <- renderedBackground
        fg <- renderedForeground
        return (bg, fg)
    )
    -- We can't update our state inside the render function, which means we have to create a new generator each time.
    -- This generator seed changes every second, this is the wanted behavior!
    -- This also means that our random animations have a FPS of 1, which is what we want.
    (mkStdGen $ floor pt)
  where
    renderLayer :: RandomGen g => TileLayer -> Rand g [Picture]
    renderLayer = renderTileGrid pt tileSet . tileGrid

    -- TODO: Parallax scroll background image based on player position
    bgLayers = filter ((==) BackgroundTileLayer . tileLayerType) layers

    bgImageList
      | Just bgImage <- background = [getImageAsset a bgImage]
      | otherwise = []

    parallaxImageList
      | Just parallaxImage <- parallax = [applyParallax $ getImageAsset a parallaxImage]
      | otherwise = []

    applyParallax :: Picture -> Picture
    applyParallax p = translate effX effY p
      where
        (nullX, nullY) = ((x - gameWidth / 2) / gameWidth, (y - gameHeight / 2) / gameHeight)
        (imgX, imgY) = imgSize p
        (maxX, maxY) = (imgX - gameWidth, imgY - gameHeight)
        effX = nullX * maxX * (-1)
        effY = nullY * maxY

        imgSize :: Picture -> (Float, Float)
        imgSize (Bitmap img) = let (w, h) = bitmapSize img in (fromIntegral w, fromIntegral h)
        imgSize _ = error "Can only get the imgSize of an Bitmap!"

    renderedBackground = concatMapM renderLayer bgLayers <&> pictures . (++ bgImageList) . (++ parallaxImageList)

    fgLayers = filter ((==) ForegroundTileLayer . tileLayerType) layers
    renderedForeground
      | Just fgImage <- foreground = rest <&> pictures . (getImageAsset a fgImage :)
      | otherwise = rest <&> pictures
      where
        rest = concatMapM renderLayer fgLayers

renderTileGrid :: RandomGen g => Float -> TileSet -> TileGrid -> Rand g [Picture]
renderTileGrid pt _ [] = return []
renderTileGrid pt ts ((_, 0) : is) = renderTileGrid pt ts is
renderTileGrid pt ts (((x, y), tile) : is) = do
  pic <- renderTile pt ts tile (x + 4, y + 4)
  theRest <- renderTileGrid pt ts is
  return $ pic : theRest

renderTile :: RandomGen g => Float -> TileSet -> Int -> Vec2 -> Rand g Picture
renderTile pt ts i xy = do
  tileToRender <- animateTile pt i
  return $
    case lookup tileToRender ts of
      Nothing -> trace ("tile not found: " ++ show i) blank
      Just pic -> setPos xy pic

animateTile :: RandomGen g => Float -> Int -> Rand g Int
animateTile pt i
  -- Acid waves (top layer)
  | i == 673 || i == 674 = Rng.uniform [673, 674]
  -- Acid 'solid' blocks, randomly add bubbles (with a small chance)
  | i == 675 = Rng.fromList ((675, 1) : map (,0.01) [717, 718, 719, 720, 761, 762, 763, 764])
  -- Laser
  | i == 457 && odd lt = return 459
  | i == 458 && odd lt = return 460
  | i == 501 && odd lt = return 503
  | i == 502 && odd lt = return 504
  | i == 545 && odd lt = return 547
  | i == 546 && odd lt = return 548
  | i == 589 && odd lt = return 591
  | i == 590 && odd lt = return 592
  -- Non animated tile
  | otherwise = return i
  where
    -- Laser t, magic numbers.
    lt = timeToFrame pt 3 2

renderCursor :: Assets -> World -> Picture
renderCursor a (World _ i) = setPos (pointer i) $ getImageAsset a "Cursor"

renderPlayer :: Float -> Assets -> World -> Player -> Picture
renderPlayer pt a w p
  -- Small offset because the player bounding box is not the same size as the texture.
  | playerHealth p > 0 = setPos (x, y - 4) pic
  | otherwise = blank
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
        frame = floor $ (pt - fromIntegral (floor pt)) * 8

renderEntities :: Float -> Assets -> [LevelEntity] -> Picture
renderEntities pt a = pictures . map renderEntity
  where
    fx = fxSheet a

    renderEntity :: LevelEntity -> Picture
    renderEntity x = setPos (center x) $ getEntityPicture x

    getEntityPicture :: LevelEntity -> Picture
    getEntityPicture LevelEntity {entityType = Bullet {bulletType = EnemyWeapon}} = enemyBullet fx
    getEntityPicture LevelEntity {entityType = Bullet {bulletType = PeaShooter}} = playerBullets fx !! 2
    getEntityPicture LevelEntity {entityType = Bullet {bulletType = RocketLauncher}, entityVelocity = vel} = fireballPicture $ fireballDirection vel
      where
        -- We could've also just rotated the sprite, oh well.
        fireballDirection :: Vec2 -> Int
        fireballDirection (x, y)
          | nullX < 0 && nullY < 0.25 && nullY > -0.25 = 0
          | nullX < 0 && nullY < -0.25 && nullY > -0.75 = 1
          | nullY < -0.75 = 2
          | nullX > 0 && nullY > -0.75 && nullY < -0.25 = 3
          | nullX > 0 && nullY > -0.25 && nullY < 0.25 = 4
          | nullX > 0 && nullY > 0.25 && nullY < 0.75 = 5
          | nullY > 0.75 = 6
          | otherwise = 7
          where
            nullX = x / len
            nullY = y / len * (-1)
            len = euclideanDistance (0, 0) (x, y)

        fireballPicture :: Int -> Picture
        fireballPicture dir = fireball fx !! (frame + (dir * 2))
        frame = timeToFrame pt 8 2
    getEntityPicture LevelEntity {entityType = Bullet {}} = head (playerBullets fx)
    getEntityPicture LevelEntity {entityType = (EffectEntity t totalLifetime lifetime)} = frame
      where
        frame
          | t == DamageExplosion = explosions (fxSheet a) !! getFrame 8
          | t == EnemyDeath = smallExplosions (fxSheet a) !! getFrame 8
          | t == RedBulletImpact = redBulletImpact (fxSheet a) !! getFrame 2
          | t == BlueBulletImpact = blueBulletImpact (fxSheet a) !! getFrame 3
          | t == PlayerDeath = playerDeath (fxSheet a) !! getFrame 7
          | otherwise = greenBulletImpact (fxSheet a) !! getFrame 3

        -- Get frame count based on how far in the fx's lifetime we are. Arg = total frames.
        getFrame :: Int -> Int
        getFrame i = min (i - 1) $ floor $ (1 - (totalLifetime - lifetime) / totalLifetime) * fromIntegral i
    getEntityPicture _ = renderDbgString red "entity not implemented"

renderEnemies :: Float -> Assets -> [EnemyInstance] -> Picture
renderEnemies pt a = pictures . map renderEnemy
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
            frame = timeToFrame pt 6 3 -- Length of crabWalkRight and crabWalkLeft
        getEnemyPicture SunEnemy
          | vx > 0 = sunIdle sheet !! (4 - frame)
          | otherwise = sunIdle sheet !! frame
          where
            frame = timeToFrame pt 10 5

renderHud :: Float -> Assets -> Font -> Player -> IO Picture
renderHud pt a f pl = do
  hpTxt <- renderString f white ("HP: " ++ show (round hp) ++ "/" ++ show (round maxHp))
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
    hp = playerHealth pl
    maxHp = playerMaxHealth pl

    getColor wpn
      | wpn == selectedWeapon = red
      | otherwise = white

renderDebugOverlay :: World -> Picture
renderDebugOverlay (World s i) =
  setPos (10, 10) $ renderDbgString green $ "timeMultiplier: " ++ show (timeMultiplier i) ++ extra s
  where
    extra (Gameplay gp) = " playTime: " ++ printf "%0.2f" (playTime gp)
    extra _ = ""

renderObjDebugOverlay :: Object2D a => a -> Picture
renderObjDebugOverlay o = pictures [boundingBox, origin, namePic]
  where
    (w, h) = size o

    namePic = setPos (position o) $ renderDbgString green $ name o
    origin = setPos (position o) $ color green $ circleSolid 1
    boundingBox = setPos (center o) $ color red $ rectangleWire w h
