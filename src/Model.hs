module Model where

import Data.Map (Map)
import Data.Set (Set, empty)
import GHC.Enum (Enum)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Interact (Key)
import SDL.Font (Font)

type Assets = Map String Picture

data Player = Player
  { health :: Int,
    maxHealth :: Int,
    -- | Set with the LevelNames of the completed levels.
    completedLevels :: Set String
  }

data MenuType = MainMenu | LevelSelectMenu

data Scene
  = Intro {displayTimer :: Float}
  | Menu
      { -- | Menu type.
        menuType :: MenuType,
        parentMenu :: Maybe Scene,
        -- | Time since last input.
        lastInput :: Float,
        -- | Index of the selected menu item.
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

instance Show Level where
  show = show . levelName

data LevelObject = LevelObject
  { levelObjectName :: String,
    levelObjectPosition :: (Int, Int), -- Or floats idk
    levelObjectSize :: (Int, Int),
    levelObjectProperties :: Map String String
  }

initWorld :: World
initWorld = World (Intro 2.5) empty (0, 0) initPlayer

initPlayer :: Player
initPlayer = Player 100 100 empty

createMenu :: MenuType -> Maybe Scene -> Scene
createMenu m p = Menu m p 0 0

initMainMenu :: Scene
initMainMenu = createMenu MainMenu Nothing
