module Coordinates where

import Graphics.Gloss
import Model

setPos :: Vec2 -> Picture -> Picture
setPos (x, y) = translate (x - gameWidth / 2) (gameHeight / 2 - y)

glossToView :: Vec2 -> Vec2 -> Vec2
glossToView (mx, my) (x, y) =
  let scaleX = mx / gameWidth; scaleY = my / gameHeight
   in ((x + mx / 2) / scaleX, (my / 2 - y) / scaleY)
