module World where

import Assets
import Common
import Coordinates
import Data.Set (empty, member)
import Input
import Menu
import Model
import Rendering
import SDL.Font (Font)

updateWorld :: [Level] -> Float -> World -> IO World
updateWorld l d w@(World s i _) =
  return $
    w
      { -- Update scene
        scene = updateScene l d w s,
        -- Clear input event list
        input = i {events = []}
      }

updateScene :: [Level] -> Float -> World -> Scene -> Scene
-- Intro Scene
updateScene _ d (World _ i _) (IntroScene dt)
  -- Skip intro screen when any key is pressed.
  | not $ null (keys i) = initMainMenu
  -- Keep displaying intro screen while counting down.
  | newDisplayTimer > 0 = IntroScene newDisplayTimer
  | otherwise = initMainMenu
  where
    newDisplayTimer = dt - d
-- Menu Scenes
updateScene l d w s@(MenuScene t _ _) = case t of
  -- Main Menu
  MainMenu ->
    let (activatedItem, s) = updateMenuScene 3 d w
     in case activatedItem of
          -- Start game
          0 -> s
          -- Level Select
          1 -> createMenu LevelSelectMenu (Just s)
          -- Quit
          2 -> s
          -- Default
          _ -> s
  -- Level Select Menu
  LevelSelectMenu ->
    let (activatedItem, s) = updateMenuScene (length l) d w
     in s
-- Default, do nothing
updateScene _ _ _ s = error "Not implemented"
