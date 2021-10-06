module World where

import Assets
import Coordinates
import Data.Set (empty, member)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Input
import Model
import Rendering
import SDL.Font (Font)

-- | Delay between each item scroll in seconds. 0 = no delay.
menuStepDelay :: Float
menuStepDelay = 0.2

menuWrapAround :: Int -> Int -> Int
menuWrapAround itemCount x
  | x < 0 = itemCount - 1
  | x >= itemCount = 0
  | otherwise = x

updateWorld :: Float -> World -> IO World
updateWorld d w = return $ w {scene = updateScene d w (scene w)}

updateScene :: Float -> World -> Scene -> Scene
updateScene d w (Intro dt)
  -- Skip intro screen when any key is pressed.
  | not $ null (keys w) = initMainMenu
  -- Keep displaying intro screen while counting down.
  | newDisplayTimer > 0 = Intro newDisplayTimer
  | otherwise = initMainMenu
  where
    newDisplayTimer = dt - d
updateScene d w s@(Menu MainMenu _ lastInput selectedItem)
  -- Enter key -> go to selected menu item
  | isKeyDown w (SpecialKey KeyEnter) =
    case selectedItem of
      -- Start game
      0 -> s
      -- Level Selector
      1 -> createMenu LevelSelectMenu (Just s)
      -- Quit game
      2 -> s
      -- Unimplemented menus
      _ -> s
  -- Up key
  | canInput && isKeyDown w (SpecialKey KeyUp) = s {lastInput = 0, selectedItem = wrap (selectedItem - 1)}
  -- Down key
  | canInput && isKeyDown w (SpecialKey KeyDown) = s {lastInput = 0, selectedItem = wrap (selectedItem + 1)}
  -- When no key is pressed
  | otherwise = s {lastInput = lastInput + d}
  where
    canInput = lastInput > menuStepDelay
    wrap = menuWrapAround 3
updateScene d w s@(Menu LevelSelectMenu (Just p) lastInput selectedItem)
  -- Go back to the main menu when the player presses Escape.
  | isKeyDown w (SpecialKey KeyEsc) = p
updateScene _ _ s = s -- Default, do nothing.

renderWorldScaled :: Assets -> Font -> World -> IO Picture
renderWorldScaled a f w = do
  world <- renderWorld a f w
  return $ scale worldScale worldScale world

-- TODO: Split this up into multiple functions.
renderWorld :: Assets -> Font -> World -> IO Picture
renderWorld a f (World Intro {} _ pt pl) = return $ getAsset "Intro" a
renderWorld a f (World (Menu MainMenu _ lastInput selectedItem) _ pt pl) = do
  -- TODO: Maybe use caching?
  gameTxt <- renderString f red "Game"
  subTxt <- renderString f white "UU-INFOFP"
  startTxt <- renderString f (getColor 0) "Start"
  levelSelectTxt <- renderString f (getColor 1) "Level Select"
  quitTxt <- renderString f (getColor 2) "Quit"
  return $
    pictures
      [ getAsset "MainMenuBg" a,
        setPos 120 60 $ scale 4 4 gameTxt,
        setPos 128 48 subTxt,
        setPos 120 94 startTxt,
        setPos 120 106 levelSelectTxt,
        setPos 120 118 quitTxt
      ]
  where
    getColor :: Int -> Color
    getColor itemIdx
      | selectedItem == itemIdx = red
      | otherwise = violet
renderWorld a f (World (Menu LevelSelectMenu _ lastInput selectedItem) _ pt pl) = do
  selectLevelTxt <- renderString f white "Select a level"
  return $
    pictures
      [ getAsset "MainMenuBg" a,
        setPos 120 24 selectLevelTxt
      ]
renderWorld _ f _ = do
  str <- renderString f red "Scene not implemented"
  return $ setPos 120 80 str
