module Main where

import Assets
import Controller
import Data.Map (empty)
import Graphics.Gloss
import Rendering
import World

createWindow :: Display
createWindow = InWindow "UU-INFOFP-Game" (gameWidth * windowScale, gameHeight * windowScale) (10, 10)

main :: IO ()
main = do
  assets <- loadAssets

  play
    createWindow -- Display mode.
    black -- Background color.
    10 -- Number of simulation steps to take for each second of real time.
    (initWorld assets) -- The initial World.
    renderWorldScaled -- An action to convert the World a picture.
    handleInput -- A function to handle input events.
    updateWorld -- A function to step the World one iteration. It is passed the period of time (in seconds) needing to be advanced.
