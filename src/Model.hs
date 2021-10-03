module Model where

import Data.Map (Map)
import Data.Set (Set)
import GHC.Enum (Enum)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Interact (Key)
import SDL.Font (Font)

type Assets = Map String Picture

data Player = Player {}

data Scene
  = Intro {displayTimer :: Float}
  | Test
  | MainMenu
      { -- | Time since last input.
        lastInput :: Float,
        -- | Index of the selected menu item.
        selectedItem :: Int
      }
  | Gameplay

data World = World
  { assets :: Assets,
    font :: Font,
    scene :: Scene,
    keys :: Set Key,
    pointer :: (Float, Float)
  }
