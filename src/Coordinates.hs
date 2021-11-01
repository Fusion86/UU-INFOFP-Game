module Coordinates where

import Graphics.Gloss
import Model

setPos :: Vec2 -> Picture -> Picture
setPos (x, y) = translate (x - gameWidth / 2) (gameHeight / 2 - y)
