module Coordinates where

import Graphics.Gloss

worldScale :: Float
worldScale = 3

worldWidth :: Float
worldWidth = 480

worldHeight :: Float
worldHeight = 320

setPos :: (Float, Float) -> Picture -> Picture
setPos (x, y) = translate (x - worldWidth / 2) (y * (-1) + worldHeight / 2)

glossToWorld :: (Float, Float) -> (Float, Float) -> (Float, Float)
glossToWorld (mx, my) (x, y) =
  let scaleX = mx / worldWidth; scaleY = my / worldHeight
   in ((x + mx / 2) / scaleX, (y * (-1) + my / 2) / scaleY)
