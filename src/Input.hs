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
addEvent e w@(World _ i@(Input _ ev _)) = w {input = i {events = dbg "events" $ e : ev}}

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
  | otherwise = addKey k w
-- Any button/key released
handleInput (EventKey k Up _ _) w = removeKey k w
-- Mouse move event
handleInput (EventMotion p) w@(World _ i) =
  w {input = i {pointer = glossToWorld (worldWidth * worldScale, worldHeight * worldScale) p}}
-- Default, ignore event
handleInput e w = w
