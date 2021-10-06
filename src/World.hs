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

initWorld :: World
initWorld = World (Intro 2.5) empty (0, 0) initPlayer

initPlayer :: Player
initPlayer = Player 100 100 0

initMainMenu :: Scene
initMainMenu = MainMenu 0 0

initChapterSelect :: Scene
initChapterSelect = ChapterSelect 0

-- | How many items there are in the main menu. Magic number.
mainMenuItemCount :: Int
mainMenuItemCount = 3

-- | Delay between each item scroll in seconds. 0 = no delay.
menuStepDelay :: Float
menuStepDelay = 0.2

updateWorld :: Float -> World -> IO World
updateWorld d w = return $ w {scene = updateScene d w (scene w)}

updateScene :: Float -> World -> Scene -> Scene
updateScene d w (Intro dt)
  | newDisplayTimer > 0 = Intro newDisplayTimer -- Keep displaying the Intro screen
  | otherwise = initMainMenu -- Go to main menu
  where
    newDisplayTimer = dt - d
updateScene d w s@(MainMenu lastInput selectedItem)
  -- Enter key -> go to selected menu item
  | isKeyDown w (SpecialKey KeyEnter) =
    case selectedItem of
      -- Start game
      0 -> s
      -- Level Viewer
      1 -> initChapterSelect
      -- Quit game
      2 -> s
      -- Unimplemented menus
      _ -> s
  -- Up key
  | canInput && isKeyDown w (SpecialKey KeyUp) = s {lastInput = 0, selectedItem = wrapAround (selectedItem - 1)}
  -- Down key
  | canInput && isKeyDown w (SpecialKey KeyDown) = s {lastInput = 0, selectedItem = wrapAround (selectedItem + 1)}
  -- When no key is pressed
  | otherwise = s {lastInput = lastInput + d}
  where
    canInput = lastInput > menuStepDelay
    wrapAround x
      | x < 0 = mainMenuItemCount - 1
      | x >= mainMenuItemCount = 0
      | otherwise = x
updateScene d w s@(ChapterSelect selectedItem)
  | isKeyDown w (SpecialKey KeyEsc) = initMainMenu -- Go back to main menu
  | otherwise = s
updateScene _ _ s = s -- Default, do nothing.

renderWorldScaled :: Assets -> Font -> World -> IO Picture
renderWorldScaled a f w = do
  world <- renderWorld a f w
  return $ scale worldScale worldScale world

-- TODO: Split this up into multiple functions.
renderWorld :: Assets -> Font -> World -> IO Picture
renderWorld a f (World s _ pt pl) = do
  case s of
    Intro _ -> return $ getAsset "Intro" a
    MainMenu _ selectedItem -> do
      -- TODO: Maybe use caching?
      gameTxt <- renderString f red "Game"
      subTxt <- renderString f white "UU-INFOFP"
      startTxt <- renderString f (getColor 0) "Start"
      chapterSelectTxt <- renderString f (getColor 1) "Chapter Select"
      quitTxt <- renderString f (getColor 2) "Quit"
      return $
        pictures
          [ getAsset "MainMenuBg" a,
            setPos 120 60 $ scale 4 4 gameTxt,
            setPos 128 48 subTxt,
            setPos 120 94 startTxt,
            setPos 120 106 chapterSelectTxt,
            setPos 120 118 quitTxt
          ]
      where
        getColor :: Int -> Color
        getColor itemIdx
          | selectedItem == itemIdx = red
          | otherwise = violet
    ChapterSelect selectedItem ->
      return $
        pictures
          [ translate (-110) (-70) (renderDbgString 0.25 green "-110,-70"),
            translate 0 0 (renderDbgString 0.25 green "0,0"),
            translate (-110) 70 (renderDbgString 0.25 green "-110,70"),
            translate 90 (-70) (renderDbgString 0.25 green "90,-70"),
            translate 90 70 (renderDbgString 0.25 green "90,70")
          ]
    Gameplay level -> return $ renderDbgString 1 red "Not implemented"
