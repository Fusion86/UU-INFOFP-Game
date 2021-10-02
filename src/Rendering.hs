module Rendering where

import Data.Text (pack)
import Data.Word (Word8)
import Graphics.Gloss
import Graphics.Gloss.SDL.Surface (CacheTexture (..), bitmapOfSurface, withSdlSurface)
import SDL.Font (Font, solid)
import SDL.Vect (V4 (..))

renderDbgString :: Float -> Color -> String -> Picture
renderDbgString size clr str =
  let x = size * 0.1
   in scale x x $ color clr $ text str

-- | Render a string with given font and color. The origin is the middle of the string.
renderString :: Font -> Color -> String -> IO Picture
renderString f c str = do
  surface <- solid f (colorCvt c) (pack str)
  ((dw, dh), bg) <- bitmapOfSurface NoCache surface
  return bg
  where
    colorCvt :: Color -> V4 Word8
    colorCvt c =
      let (r, g, b, a) = rgbaOfColor c
       in V4 (floor $ r * 255) (floor $ g * 255) (floor $ b * 255) (floor $ a * 255)
