module Coordinates where

import Graphics.Gloss
import Model

worldScale :: Float
worldScale = 3

worldWidth :: Float
worldWidth = 480

worldHeight :: Float
worldHeight = 320

setPos :: Vec2 -> Picture -> Picture
setPos (x, y) = translate (x - worldWidth / 2) (worldHeight / 2 - y)

glossToWorld :: Vec2 -> Vec2 -> Vec2
glossToWorld (mx, my) (x, y) =
  let scaleX = mx / worldWidth; scaleY = my / worldHeight
   in ((x + mx / 2) / scaleX, (my / 2 - y) / scaleY)
