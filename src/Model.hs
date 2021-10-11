module Model where

import Data.Map (Map)
import Data.Set (Set, empty)
import GHC.Enum (Enum)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Interact (Key)
import SDL.Font (Font)

type Assets = Map String Picture

type TileSet = Map Int Picture

data Player = Player
  { health :: Int,
    maxHealth :: Int,
    -- | A Set with the LevelNames of the completed levels.
    completedLevels :: Set String
  }
  deriving (Show)

data MenuType = MainMenu | LevelSelectMenu
  deriving (Show, Eq)

data Scene
  = IntroScene {displayTimer :: Float}
  | MenuScene
      { -- | Menu type.
        menuType :: MenuType,
        parentMenu :: Maybe Scene,
        -- | Index of the selected menu item.
        selectedItem :: Int
      }
  | Gameplay
      { level :: Level
      }
  deriving (Show)

data World = World
  { scene :: Scene,
    input :: Input,
    player :: Player
  }
  deriving (Show)

data InputEvent
  = MenuDown
  | MenuUp
  | MenuEnter
  | MenuBack
  deriving (Show, Eq)

data Input = Input
  { keys :: Set Key,
    events :: [InputEvent],
    pointer :: (Float, Float)
  }
  deriving (Show)

data Level = Level
  { levelName :: String,
    levelBackground :: String,
    -- | List of tile IDs, where 0 is no tile.
    tileLayer :: [[Int]],
    levelObjects :: [LevelObject]
  }
  deriving (Show)

data LevelObject = LevelObject
  { levelObjectName :: String,
    levelObjectPosition :: (Int, Int), -- Or floats idk
    levelObjectSize :: (Int, Int),
    levelObjectProperties :: Map String String
  }
  deriving (Show)

initWorld :: World
initWorld = World (IntroScene 2.5) (Input empty [] (0, 0)) initPlayer

initPlayer :: Player
initPlayer = Player 100 100 empty

createMenu :: MenuType -> Maybe Scene -> Scene
createMenu m p = MenuScene m p 0

initMainMenu :: Scene
initMainMenu = createMenu MainMenu Nothing
