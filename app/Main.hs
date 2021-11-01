module Main where

import Assets
import Common
import Coordinates
import Data.Map (empty)
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Game (Event, playIO)
import Input
import Levels
import Model
import Rendering
import SDL.Font (initialize, load)
import System.FilePath (joinPath, (</>))
import TileSet
import World

createWindow :: (Int, Int) -> Display
createWindow wh = InWindow "UU-INFOFP-Game" wh (0, 0)

main :: IO ()
main = do
  -- Eager load all assets
  !assets <- loadAssets "assets"
  !levels <- loadLevels $ "assets" </> "levels"
  !tileSet <- loadTileSet $ "assets" </> "TileSet.png"

  -- Text init and load font
  initialize
  !font <- load ("assets" </> "PressStart2P.ttf") 8

  -- Figure out how large our window can be, but still scaled to our game.
  -- Kinda shitty code, could be improved.
  -- This is also somewhat duplicated code from the `handleInput (EventResize _)` case
  (wMax, hMax) <- getScreenSize
  let scale = floorF (min (fromIntegral wMax / gameWidth) (fromIntegral hMax / gameHeight))
  let wndSize = (floor (gameWidth * scale), floor (gameHeight * scale))

  playIO
    (createWindow wndSize) -- Display mode.
    violet -- Background color.
    60 -- Number of simulation steps to take for each second of real time.
    initWorld -- The initial World.
    (renderWorldScaled assets font tileSet levels) -- An action to convert the World a picture.
    handleInputIO -- A function to handle input events.
    (updateWorldIO levels) -- A function to step the World one iteration. It is passed the period of time (in seconds) needing to be advanced.
  where
    handleInputIO e w = return $ handleInput e w
    updateWorldIO l d w = return $ updateWorld l d w
