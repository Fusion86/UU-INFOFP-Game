module World (updateWorld) where

import Assets
import Collision
import Common
import Coordinates
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
    jumpCount = playerJumpCount pl
    lvlObjs = levelObjects $ level $ levelInstance s

    newPlayer
      -- Change state to IdleState when the player hasn't moved.
      | newPlayerPosition == (x, y) = pl {playerState = IdleState, playerJumpCount = newJumpCount}
      | otherwise = pl {playerPosition = newPlayerPosition, playerState = MovingState, playerJumpCount = newJumpCount}
    playerSize = (16, 16)

    onGround :: Bool 
    onGround = not $ validMove (x, y + 0.1) playerSize

    -- TODO: Write better code
    newJumpCount :: Int 
    newJumpCount | jumpCount > 0 = jumpCount - 1
                 | onGround && isKeyDown i (Char 'w') = 10
                 | onGround = 0
                 | otherwise = -1  

    forceLeft = if isKeyDown i (Char 'a') then -100 else 0
    forceRight = if isKeyDown i (Char 'd') then 100 else 0
    forceUp = if newJumpCount > 0 then -400 else 0 -- TODO: Make the jumpCount and force more balanced
    forceDown = if isKeyDown i (Char 's') then 200 else 100 -- TODO: Gravity
    newPlayerX = x + (forceLeft + forceRight) * d
    newPlayerY = y + (forceUp + forceDown) * d
    newPlayerX' = x + (forceLeft + forceRight) * (d / 2)
    newPlayerY' = y + (forceUp + forceDown) * (d / 2)
    newPlayerY'' = y + (forceUp + forceDown) * (d / 4)

    -- TODO: Like honestly, please clean this up :(
    -- (Though I guess it works fine, so maybe don't touch it idk)
    newPlayerPosition
      -- If the move is valid, return the new position.
      | validMove (newPlayerX - 8, newPlayerY - 4) playerSize =
        (newPlayerX, newPlayerY)
      -- Half step - If the move is valid, return the new position.
      | validMove (newPlayerX' - 8, newPlayerY' - 4) playerSize =
        (newPlayerX', newPlayerY')
      -- Half step - If the move is valid, return the new position.
      | validMove (newPlayerX' - 8, newPlayerY'' - 4) playerSize =
        (newPlayerX', newPlayerY'')
      --
      -- Only check collision on the X axis.
      -- This is needed to allow the player to move when standing on the ground.
      -- If the move is valid, return the new position.
      | validMove (newPlayerX - 8, y - 4) playerSize =
        (newPlayerX, y)
      -- Half step - If the move is valid, return the new position.
      | validMove (newPlayerX' - 8, y - 4) playerSize =
        (newPlayerX', y)
      --
      -- Only check collision on the Y axis.
      -- This is needed to allow the player to "climb walls".
      -- If the move is valid, return the new position.
      | validMove (x - 8, newPlayerY - 4) playerSize =
        (x, newPlayerY)
      -- Half step - If the move is valid, return the new position.
      | validMove (x - 8, newPlayerY' - 4) playerSize =
        (x, newPlayerY')
      -- If not a valid move, just return the old position.
      | otherwise = (x, y)

    collisionObjects :: [LevelObject]
    collisionObjects = filter ((==) "Collision" . objectName) lvlObjs

    -- Returns true when the newPlayerPosition is a valid move.
    validMove :: Vec2 -> Vec2 -> Bool
    validMove pos@(x, y) size@(w, h)
      | x < 0 || y < 0 || x + w > worldWidth || y + h > worldHeight = False
      | otherwise = not $ any (intersects pos size) collisionObjects
