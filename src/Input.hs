module Input (isKeyDown, handleInput) where

import Common
import Coordinates
import Data.Set (delete, insert, member)
import Graphics.Gloss.Interface.IO.Game
import Model

addKey :: Key -> World -> World
addKey k w@(World _ i) = trace ("keyDown: " ++ show k) w {input = i {keys = insert k (keys i)}}

removeKey :: Key -> World -> World
removeKey k w@(World _ i) = trace ("keyUp: " ++ show k) w {input = i {keys = delete k (keys i)}}

addEvent :: InputEvent -> World -> World
addEvent e w@(World _ i@Input {events = ev}) = w {input = i {events = dbg "events" $ e : ev}}

toggleDebug :: World -> World
toggleDebug w@(World _ i@Input {debugMode = d}) = w {input = i {debugMode = not d}}

toggleTimeMultiplier :: World -> World
toggleTimeMultiplier w@(World _ i@Input {timeMultiplier = m}) =
  w {input = i {timeMultiplier = new}}
  where
    new
      | m == 0 = 1
      | m < 0.20 = 0
      | otherwise = m / 2

isKeyDown :: Input -> Key -> Bool
isKeyDown i k = member k (keys i)

menuKeyMap :: Key -> Maybe InputEvent
menuKeyMap (SpecialKey KeyUp) = Just MenuUp
menuKeyMap (SpecialKey KeyDown) = Just MenuDown
menuKeyMap (SpecialKey KeyEsc) = Just MenuBack
menuKeyMap (SpecialKey KeyEnter) = Just MenuEnter
menuKeyMap (SpecialKey KeySpace) = Just MenuEnter
menuKeyMap (Char 'w') = Just MenuUp
menuKeyMap (Char 's') = Just MenuDown
menuKeyMap _ = Nothing

handleInput :: Event -> World -> World
handleInput (EventKey k Down _ _) w
  | Just ev <- menuKeyMap k = addEvent ev $ addKey k w
  | k == SpecialKey KeyF1 = toggleDebug $ addKey k w
  | k == SpecialKey KeyF2 = toggleTimeMultiplier $ addKey k w
  | otherwise = addKey k w
-- Any button/key released
handleInput (EventKey k Up _ _) w = removeKey k w
-- Mouse move event
handleInput (EventMotion p) w@(World _ i) =
  w {input = i {pointer = glossToView (viewWidth, viewHeight) p}}
-- Default, ignore event
handleInput e w = w
