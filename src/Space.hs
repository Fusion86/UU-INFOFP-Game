module Space where

import Graphics.Gloss

windowScale :: Int
windowScale = 4

gameWidth :: Float
gameWidth = 240

gameHeight :: Float
gameHeight = 160

setPos :: Float -> Float -> Picture -> Picture
setPos x y p = translate (x - gameWidth / 2) (y * (-1) + gameHeight / 2) p
