module Main where

import Assets
import Controller
import Data.Map (empty)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Rendering
import SDL.Font
import Space
import World

createWindow :: Display
createWindow =
  let w = round gameWidth; h = round gameHeight
   in InWindow "UU-INFOFP-Game" (w * windowScale, h * windowScale) (100, 100)

main :: IO ()
main = do
  assets <- loadAssets "assets"

  -- Text init and load font
  initialize
  font <- load "assets/PressStart2P.ttf" 8

  let world = initWorld assets font

  playIO
    createWindow -- Display mode.
    black -- Background color.
    10 -- Number of simulation steps to take for each second of real time.
    world -- The initial World.
    renderWorldScaled -- An action to convert the World a picture.
    handleInput -- A function to handle input events.
    updateWorld -- A function to step the World one iteration. It is passed the period of time (in seconds) needing to be advanced.
