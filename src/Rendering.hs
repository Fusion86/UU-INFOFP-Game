module Rendering where

import Coordinates
import Data.Text (pack)
import Data.Word (Word8)
import Graphics.Gloss
import Graphics.Gloss.SDL.Surface (CacheTexture (..), bitmapOfSurface, withSdlSurface)
import SDL.Font (Font, solid)
import SDL.Vect (V4 (..))

renderDbgString :: Color -> String -> Picture
renderDbgString clr str =
  let x = 0.1
   in scale x x $ color clr $ text str

-- | Render a string with given font and color. The origin is the middle of the string.
-- | Does not cache, and maybe it leaks memory idk.
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

renderMenuItems :: Font -> Int -> [String] -> IO [Picture]
renderMenuItems font selectedIndex xs = sequence (helper 0 xs)
  where
    -- TODO: Cleanup spaghetti code
    -- Returns the index of each item that is visible in the menu.
    visibleItemRange =
      let range = take visibleItemCount (drop dropCount [0 .. (length xs - 1)])
       in range
      where
        -- Number of visible items in the menu.
        visibleItemCount = 9

        dropCount
          | selectedIndex >= length xs - visibleItemCount = min (selectedIndex - visibleItemCount `div` 2) 8
          | otherwise = max 0 (selectedIndex - visibleItemCount `div` 2)

    helper :: Int -> [String] -> [IO Picture]
    helper _ [] = []
    helper currentIndex (x : xs)
      -- Skip items that aren't in the visible item range (scroll view).
      | currentIndex `notElem` visibleItemRange = rest
      -- If the item is the selected item highlight it red.
      | currentIndex == selectedIndex = renderString font red x : rest
      -- Otherwise just render it the default color (white for now).
      | otherwise = renderString font white x : rest
      where
        rest = helper (currentIndex + 1) xs

-- | Render a horizontal list starting at `start`.
-- Each next element will be `spacing` px lower than the element before it.
renderList :: (Float, Float) -> Float -> [Picture] -> Picture
renderList start spacing = pictures . helper start
  where
    helper :: (Float, Float) -> [Picture] -> [Picture]
    helper _ [] = []
    helper (x, y) (p : ps) = setPos (x, y) p : helper (x, y + spacing) ps
