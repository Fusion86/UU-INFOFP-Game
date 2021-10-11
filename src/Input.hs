-- {-# LANGUAGE Strict #-}

module Input (isKeyDown, handleInput) where

import Common
import Coordinates
import Data.Set (delete, insert, member)
import Graphics.Gloss.Interface.IO.Game
import Model

addKey :: Key -> World -> World
addKey k w@(World _ i) = w {input = i {keys = insert (dbg "keyDown" k) (keys i)}}

removeKey :: Key -> World -> World
removeKey k w@(World _ i) = w {input = i {keys = delete (dbg "keyUp" k) (keys i)}}

addEvent :: InputEvent -> World -> World
addEvent e w@(World _ i@(Input _ es _)) = w {input = i {events = dbg "events" $ e : es}}

isKeyDown :: Input -> Key -> Bool
isKeyDown i k = member k (keys i)

handleInput :: Event -> World -> IO World
-- Up Arrow pressed
handleInput (EventKey k@(SpecialKey KeyUp) Down _ _) w = return $ addEvent MenuUp $ addKey k w
-- Down Arrow pressed
handleInput (EventKey k@(SpecialKey KeyDown) Down _ _) w = return $ addEvent MenuDown $ addKey k w
-- Esc pressed
handleInput (EventKey k@(SpecialKey KeyEsc) Down _ _) w = return $ addEvent MenuBack $ addKey k w
-- Enter pressed
handleInput (EventKey k@(SpecialKey KeyEnter) Down _ _) w = return $ addEvent MenuEnter $ addKey k w
-- Any button/key pressed
handleInput (EventKey k Down _ _) w = return $ addKey k w
-- Any button/key released
handleInput (EventKey k Up _ _) w = return $ removeKey k w
-- Mouse move event
handleInput (EventMotion p) w@(World _ i) =
  return $ w {input = i {pointer = glossToWorld (worldWidth * worldScale, worldHeight * worldScale) p}}
-- Default, ignore event
handleInput e w = return w
