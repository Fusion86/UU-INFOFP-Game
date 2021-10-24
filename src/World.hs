module World (updateWorld) where

import Assets
import Collision
import Common
import Coordinates
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Set (empty, member)
import Graphics.Gloss.Interface.IO.Game
import Input
import Menu
import Model
import Rendering
import SDL.Font (Font)
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

updateWorld :: [Level] -> Float -> World -> World
updateWorld l d w@(World s i) =
  w
    { -- Update scene
      scene = updateScene l d w,
      -- Clear input event list
      input = i {events = []}
    }

updateScene :: [Level] -> Float -> World -> Scene
-- Intro Scene
updateScene _ d (World (IntroScene dt) i)
  -- Skip intro screen when any key is pressed.
  | not $ null (keys i) = initMainMenu
  -- Keep displaying intro screen while counting down.
  | newDisplayTimer > 0 = IntroScene newDisplayTimer
  | otherwise = initMainMenu
  where
    newDisplayTimer = dt - d
-- Menu Scenes
updateScene l d w@(World s@(MenuScene menuType parentMenu _) _) =
  case menuType of
    -- Main Menu
    MainMenu ->
      let (activatedItem, s) = updateMenuScene 3 d w
       in case activatedItem of
            -- Start game, currently always starts the first level.
            Just 0 -> createGameplay (head l) initPlayer
            -- Level Select
            Just 1 -> createMenu LevelSelectMenu (Just s)
            -- Quit
            -- I don't want to introduce IO for the whole hierarchy just because of this one function.
            Just 2 -> unsafePerformIO exitSuccess
            -- Default
            _ -> s
    -- Level Select Menu
    LevelSelectMenu ->
      let (activatedItem, s) = updateMenuScene (length l) d w
       in case activatedItem of
            Nothing -> s
            Just x -> createGameplay (l !! x) initPlayer
    -- Gameplay paused menu
    PauseMenu ->
      let (activatedItem, s) = updateMenuScene 2 d w
       in case activatedItem of
            -- Resume
            Just 0 -> fromMaybe s parentMenu
            -- Quit
            Just 1 -> initMainMenu
            -- Default
            _ -> s
    -- End of level menu, should show score etc.
    EndOfLevel -> s
-- Gameplay
-- NOTE: "lens/optics provide a language to do this pattern matching." -- dminuoso
updateScene _ d w@(World s@Gameplay {} _)
  | MenuBack `elem` events i = createMenu PauseMenu (Just s)
  | otherwise = s {player = newPlayer, playTime = pt + d}
  where
    i = input w
    pt = playTime $ scene w
    pl = player $ scene w
    (x, y) = playerPosition pl
    (vx, vy) = playerVelocity pl
    state = playerState pl
    lvlObjs = levelObjects $ level $ levelInstance s

    gravity = 10

    newPlayer
      -- Change state to IdleState when the player hasn't moved.
      | newPlayerPosition == (x, y) = pl {playerState = IdleState, playerVelocity = newVelocity}
      | otherwise = pl {playerPosition = newPlayerPosition, playerState = MovingState, playerVelocity = newVelocity}
    playerSize = (16, 16)

    velocityY = tmp + gravity
      where
        tmp
          | onGround && jumpKeyDown = -300
          | canClimb && jumpKeyDown = -100
          | onGround = 0
          | not onGround && not canJumpHigher = 0
          | otherwise = vy

        jumpKeyDown = isKeyDown i (Char 'w') || isKeyDown i (SpecialKey KeySpace)

    velocityX
      | isKeyDown i (Char 'a') = max (-100) vxl
      | isKeyDown i (Char 'd') = min 100 vxr
      | otherwise = 0
      where
        vxl
          | vx > -25 = -25
          -- This does NOT work
          -- otherwise = dbg "otherwise" $ vx ** 1.05
          | otherwise = (abs vx ** 1.05) * (-1)
        vxr
          | vx < 25 = 25
          | otherwise = vx ** 1.05

    newVelocity = (velocityX, velocityY)

    -- newPlayerX = x + (velocityX * d)
    -- newPlayerY = y + (velocityY + gravity * d)

    newPlayerPosition = (newPlayerX, newPlayerY)
      where
        -- Finds the best move we can do. Kinda brute-force, but hey it works.
        newPlayerX =
          fromMaybe x $
            find validMoveX $
              map (\z -> x + (velocityX * z)) [d, d / 2, d / 3, d / 4]

        newPlayerY =
          fromMaybe y $
            find validMoveY $
              map (\z -> y + (velocityY * z)) [d, d / 2, d / 3, d / 4]

    validMoveX z = validMove (z - 8, y - 4) playerSize
    validMoveY z = validMove (x - 8, z - 4) playerSize

    collisionObjects :: [LevelObject]
    collisionObjects = filter ((==) "Collision" . objectName) lvlObjs

    validMove :: Vec2 -> Vec2 -> Bool
    validMove pos@(x, y) size@(w, h)
      | x < 0 || y < 0 || x + w > worldWidth || y + h > worldHeight = False
      | otherwise = not $ any (intersects pos size) collisionObjects

    canClimb :: Bool
    canClimb = not (validMove (x - 10, y - 4 + 1.5) playerSize) || not (validMove (x - 6, y - 4 + 1.5) playerSize)

    onGround :: Bool
    onGround = not $ validMoveY (y + 1.5)

    canJumpHigher :: Bool
    canJumpHigher = validMoveY (y - 1.2)
