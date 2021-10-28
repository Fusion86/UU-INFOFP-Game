module Coordinates where

import Graphics.Gloss
import Model

gameWidth :: Float
gameWidth = 576 -- 8 * 72

gameHeight :: Float
gameHeight = 336 -- 8 * 42

viewScale :: Float
viewScale = 3 -- For 1440p displays you want to set this to 4

viewWidth :: Float
viewWidth = gameWidth * viewScale

viewHeight :: Float
viewHeight = gameHeight * viewScale

setPos :: Vec2 -> Picture -> Picture
setPos (x, y) = translate (x - gameWidth / 2) (gameHeight / 2 - y)

glossToView :: Vec2 -> Vec2 -> Vec2
glossToView (mx, my) (x, y) =
  let scaleX = mx / gameWidth; scaleY = my / gameHeight
   in ((x + mx / 2) / scaleX, (my / 2 - y) / scaleY)
