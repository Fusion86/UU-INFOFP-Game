module Colors where

import Graphics.Gloss (Color, makeColor)

fromRgba :: Int -> Int -> Int -> Int -> Color
fromRgba r g b a = makeColor (fromIntegral r / 255) (fromIntegral g / 255) (fromIntegral b / 255) (fromIntegral a / 255)

red :: Color
red = fromRgba 237 28 36 255

darkRed :: Color
darkRed = fromRgba 136 0 21 255

white :: Color
white = fromRgba 255 255 255 255
