module Rendering where

import Graphics.Gloss

windowScale :: Int
windowScale = 4

gameWidth :: Int
gameWidth = 240

gameHeight :: Int
gameHeight = 160

renderString :: Float -> Color -> String -> Picture
renderString size clr str =
  let x = size * 0.1
   in scale x x (color clr (text str))
