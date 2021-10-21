module World (updateWorld) where

import Assets
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
updateScene
  _
  d
  ( World
      s@( Gameplay
            (LevelInstance (Level _ _ _ layers levelObjects) _ _)
            player@(Player _ _ _ _ _ _ (x, y))
            pt
          )
      i@(Input k ev p)
    )
    | MenuBack `elem` ev = createMenu PauseMenu (Just s)
    | otherwise = s {player = newPlayer, playTime = pt + d}
    where
      newPlayer = player {playerPosition = newPlayerPosition}
      playerSize = (16, 16)

      -- TODO: Write better code
      forceLeft = if isKeyDown i (Char 'a') then -100 else 0
      forceRight = if isKeyDown i (Char 'd') then 100 else 0
      forceUp = if isKeyDown i (Char 'w') then -100 else 0 -- TODO: Check if player can jump
      forceDown = if isKeyDown i (Char 's') then 100 else 50 -- TODO: Gravity
      newPlayerX = x + (forceLeft + forceRight) * d
      newPlayerY = y + (forceUp + forceDown) * d
      newPlayerX' = x + (forceLeft + forceRight) * (d / 2)
      newPlayerY' = y + (forceUp + forceDown) * (d / 2)

      newPlayerPosition
        -- If the move is valid, return the new position.
        | validMove (newPlayerX - 8, newPlayerY - 4) playerSize =
          (newPlayerX, newPlayerY)
        -- If the move is valid, return the new position.
        | validMove (newPlayerX' - 8, newPlayerY' -4) playerSize =
          (newPlayerX', newPlayerY')
        -- If the move is valid, return the new position.
        | validMove (newPlayerX - 8, y - 8) playerSize =
          (newPlayerX, y)
        -- If not a valid move, just return the old position.
        | otherwise = (x, y)

      -- Returns true when the newPlayerPosition is a valid move.
      validMove :: Vec2 -> Vec2 -> Bool
      validMove pos@(x, y) size@(w, h)
        | x < 0 || y < 0 || x + w > worldWidth || y + h > worldHeight = False
        | otherwise = not $ any (intersects pos size) collisionObjects

      intersects :: Vec2 -> Vec2 -> LevelObject -> Bool
      intersects pos@(x1, y1) (w1, h1) (LevelObject _ (x2, y2) (w2, h2)) =
        x1 < x2 + w2
          && x1 + w1 > x2
          && y1 < y2 + h2
          && h1 + y1 > y2

      collisionObjects :: [LevelObject]
      collisionObjects = filter ((==) "Collision" . objectName) levelObjects
