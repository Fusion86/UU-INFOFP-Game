module World where

import Assets
import Common
import Coordinates
import Data.Set (empty, member)
import Debug.Trace (trace)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
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
updateScene l d w s@(MenuScene t _ _ _) = case t of
  -- Main Menu
  MainMenu ->
    let (activatedItem, s) = updateMenuScene 3 w
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
    let (activatedItem, s) = updateMenuScene (length l) w
     in s
-- Default, do nothing
updateScene _ _ _ s = error "Not implemented"

-- updateScene _ d (World _ i _) s@(MenuScene MainMenu _ lastInput selectedItem)
--   -- Enter key -> go to selected menu item
--   | isKeyDown i (SpecialKey KeyEnter) =
--     case selectedItem of
--       -- Start game
--       0 -> s
--       -- Level Selector
--       1 -> createMenu LevelSelectMenu (Just s)
--       -- Quit game
--       2 -> s
--       -- Unimplemented menus
--       _ -> s
--   -- Up key
--   | canInput && isKeyDown i (SpecialKey KeyUp) = s {lastInput = 0, selectedItem = wrap (selectedItem - 1)}
--   -- Down key
--   | canInput && isKeyDown i (SpecialKey KeyDown) = s {lastInput = 0, selectedItem = wrap (selectedItem + 1)}
--   -- When no key is pressed
--   | otherwise = s {lastInput = lastInput + d}
--   where
--     canInput = lastInput > menuStepDelay
--     wrap = menuWrapAround 3
-- updateScene l d (World _ i _) s@(MenuScene LevelSelectMenu (Just p) lastInput selectedItem)
--   -- Go back to the main menu when the player presses Escape.
--   | isKeyDown i (SpecialKey KeyEsc) = p
--   | canInput && isKeyDown i (SpecialKey KeyUp) = s {lastInput = 0, selectedItem = wrap (selectedItem - 1)}
--   | canInput && isKeyDown i (SpecialKey KeyDown) = s {lastInput = 0, selectedItem = wrap (selectedItem + 1)}
--   | otherwise = s {lastInput = lastInput + d}
--   where
--     canInput = lastInput > menuStepDelay
--     wrap = menuWrapAround (length l)

renderWorldScaled :: Assets -> Font -> [Level] -> World -> IO Picture
renderWorldScaled a f l w = do
  world <- renderWorld a f l w
  return $ scale worldScale worldScale world

-- TODO: Split this up into multiple functions.
renderWorld :: Assets -> Font -> [Level] -> World -> IO Picture
renderWorld a f _ (World IntroScene {} _ pl) = return $ getAsset a "Intro"
renderWorld a f _ (World (MenuScene MainMenu _ lastInput selectedItem) _ pl) = do
  -- TODO: Maybe use caching?
  gameTxt <- renderString f red "Game"
  subTxt <- renderString f white "UU-INFOFP"
  menuTxts <- renderMenuItems f selectedItem ["Start", "Level Select", "Quit"]
  return $
    pictures
      [ getAsset a "MainMenuBg",
        setPos (120, 60) $ scale 4 4 gameTxt,
        setPos (128, 48) subTxt,
        renderList (120, 94) 12 menuTxts
      ]
  where
    getColor :: Int -> Color
    getColor itemIdx
      | selectedItem == itemIdx = red
      | otherwise = violet
renderWorld a f l (World (MenuScene LevelSelectMenu _ lastInput selectedItem) _ pl) = do
  selectLevelTxt <- renderString f white "Select a level"
  levelTxts <- renderMenuItems f selectedItem (map levelName l)
  return $
    pictures
      [ getAsset a "MainMenuBg",
        setPos (120, 24) selectLevelTxt,
        renderList (120, 44) 12 levelTxts
      ]
renderWorld _ f _ _ = do
  str <- renderString f red "Scene not implemented"
  return $ setPos (120, 80) str
