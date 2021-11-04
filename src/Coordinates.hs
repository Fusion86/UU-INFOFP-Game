module Coordinates where

import Graphics.Gloss
import Model

setPos :: Vec2 -> Picture -> Picture
setPos = uncurry translate . gameToGloss

gameToGloss :: Vec2 -> Vec2
gameToGloss (x, y) = (x - gameWidth / 2, gameHeight / 2 - y)
