module Input where

import Coordinates
import Data.Set (delete, insert, member)
import Debug.Trace (trace)
import Graphics.Gloss.Interface.IO.Game
import Model

handleInput :: Event -> World -> IO World
handleInput (EventKey k Down _ _) w = return $ trace ("keyDown: " ++ show k) $ w {keys = insert k (keys w)}
handleInput (EventKey k Up _ _) w = return $ trace ("keyUp: " ++ show k) $ w {keys = delete k (keys w)}
handleInput (EventMotion p) w = return $ w {pointer = glossToWorld (mx, my) p}
  where
    (mx, my) = (worldWidth * worldScale, worldHeight * worldScale)
handleInput e w = return w -- Default, ignore event.

isKeyDown :: World -> Key -> Bool
isKeyDown w k = member k (keys w)
