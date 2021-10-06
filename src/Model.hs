module Model where

import Data.Map (Map)
import Data.Set (Set)
import GHC.Enum (Enum)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Interact (Key)
import SDL.Font (Font)

type Assets = Map String Picture

data Player = Player
  { health :: Int,
    maxHealth :: Int,
    completedLevels :: Int
  }

data Scene
  = Intro {displayTimer :: Float}
  | MainMenu
      { -- | Time since last input.
        lastInput :: Float,
        -- | Index of the selected menu item.
        selectedItem :: Int
      }
  | ChapterSelect
      { -- | Index of the selected menu item.
        selectedItem :: Int
      }
  | Gameplay
      { level :: Maybe LevelObject
      }

data World = World
  { scene :: Scene,
    keys :: Set Key,
    pointer :: (Float, Float),
    player :: Player
  }

data Level = Level
  { levelName :: String,
    -- | List of tile IDs, where 0 is no tile.
    tileLayer :: [Int],
    levelObjects :: [LevelObject]
  }

data LevelObject = LevelObject
  { levelObjectName :: String,
    levelObjectPosition :: (Int, Int), -- Or floats idk
    levelObjectSize :: (Int, Int),
    levelObjectProperties :: Map String String
  }

instance Show Level where
  show l = "todo"
