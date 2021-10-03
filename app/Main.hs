module Main where

import Assets
import Coordinates
import Data.Map (empty)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (playIO)
import Input
import Rendering
import SDL.Font (initialize, load)
import World

createWindow :: Display
createWindow =
  let w = worldWidth * worldScale; h = worldHeight * worldScale
   in InWindow "UU-INFOFP-Game" (round w, round h) (100, 100)

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
    60 -- Number of simulation steps to take for each second of real time.
    world -- The initial World.
    renderWorldScaled -- An action to convert the World a picture.
    handleInput -- A function to handle input events.
    updateWorld -- A function to step the World one iteration. It is passed the period of time (in seconds) needing to be advanced.
