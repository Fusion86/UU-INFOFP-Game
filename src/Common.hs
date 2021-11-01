module Common where

import Debug.Pretty.Simple (pTraceOpt)
import Graphics.Gloss (Color, Picture, color, scale, text)
import Text.Pretty.Simple (defaultColorOptionsDarkBg)
import Text.Pretty.Simple.Internal.Printer
  ( CheckColorTty (CheckColorTty),
    OutputOptions (OutputOptions),
    StringOutputStyle (EscapeNonPrintable),
  )

-- | Counts how many times an object occurs in the list.
-- Taken from https://stackoverflow.com/a/29307068/2125072
count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

trace :: String -> a -> a
trace =
  pTraceOpt
    CheckColorTty
    -- The outputOptionsPageWidth is set to a very large number because it otherwise prints each element on a new line.
    -- Which is a bit awkward when printing a list with 600 items.
    (OutputOptions 2 99999999 True True 0 (Just defaultColorOptionsDarkBg) EscapeNonPrintable)

dbg :: Show a => String -> a -> a
dbg str a = trace (str ++ ": " ++ show a) a

safeHead :: [a] -> Maybe a
safeHead a = case a of
  [] -> Nothing
  x -> Just $ head x

renderDbgString :: Color -> String -> Picture
renderDbgString clr str =
  let x = 0.05
   in scale x x $ color clr $ text str

floorF :: Float -> Float
floorF = fromIntegral . floor
