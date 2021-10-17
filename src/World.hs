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
            (LevelInstance (Level _ _ _ layers _) _ _)
            player@(Player _ _ _ _ _ _ (x, y))
            pt
          )
      i@(Input k ev p)
    )
    | MenuBack `elem` ev = createMenu PauseMenu (Just s)
    | otherwise = s {player = newPlayer, playTime = pt + d}
    where
      newPlayer = player {playerPosition = newPlayerPosition}

      -- TODO: Write better code
      forceLeft = if isKeyDown i (Char 'a') then -100 else 0
      forceRight = if isKeyDown i (Char 'd') then 100 else 0
      forceUp = if isKeyDown i (Char 'w') then -100 else 0 -- TODO: Check if player can jump
      forceDown = if isKeyDown i (Char 's') then 100 else 0 -- TODO: Gravity
      newPlayerX = x + (forceLeft + forceRight) * d
      newPlayerY = y + (forceUp + forceDown) * d

      newPlayerPosition = (newPlayerX, newPlayerY)
