module Model where

import Data.Map (Map, empty)
import qualified Data.Set as S (Set, empty)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Interact (Key)
import SDL.Font (Font)

type Assets = Map String Picture

type TileSet = Map Int Picture

type Vec2 = (Float, Float)

data World = World
  { scene :: Scene,
    input :: Input
  }
  deriving (Show)

data Scene
  = IntroScene {displayTimer :: Float}
  | MenuScene
      { menuType :: MenuType,
        -- | The parent scene, this is the Scene which will be show when the player presses Esc to go back.
        -- E.g. to unpause the game or to go to a parent menu.
        parentScene :: Maybe Scene,
        -- | Index of the selected menu item. Has to be an Int because the range is not known at compile time.
        selectedItem :: Int
      }
  | Gameplay
      { -- | An instance of the level which is currently being played.
        levelInstance :: LevelInstance,
        -- | The current player. The player is not stored inside the LevelInstance because the player is supposed to carry over their upgrades, ammo, etc when moving between maps.
        player :: Player,
        -- | Play time in seconds.
        playTime :: Float
      }
  deriving (Show)

data MenuType = MainMenu | LevelSelectMenu | PauseMenu | EndOfLevel
  deriving (Show, Eq)

data Input = Input
  { -- | A set of the keys currently being pressed.
    keys :: S.Set Key,
    -- | Input events. Each key press corresponds to one event, which also means that multiple key presses produce multiple events.
    events :: [InputEvent],
    -- | The location of the mouse pointer, normalized to our worldWidth and worldHeight.
    pointer :: Vec2
  }
  deriving (Show)

data InputEvent
  = MenuDown
  | MenuUp
  | MenuEnter
  | MenuBack
  deriving (Show, Eq)

data Player = Player
  { -- | Current player health.
    playerHealth :: Int,
    -- | Default of 100, can be temporarily increased with upgrades.
    playerMaxHealth :: Int,
    -- | Used as a base for damage calculation, can be increased with upgrades.
    playerDamage :: Int,
    -- | How high the player can jump, can be increased with upgrades.
    playerJumpHeight :: Int,
    -- | Ammo for each weapon type. The AssaultRifle has infinite ammo (as it is considered the 'default' weapon).
    playerAmmo :: Map WeaponType Int,
    -- | Currently active weapon.
    playerSelectedWeapon :: WeaponType,
    -- | The player's position within the current active level instance.
    playerPosition :: Vec2
  }
  deriving (Show)

data WeaponType
  = AssaultRifle
  | RocketLauncher
  | Shotgun
  deriving (Show, Eq)

data Level = Level
  { levelName :: String,
    levelBackground :: Maybe String,
    levelForeground :: Maybe String,
    layers :: [TileLayer],
    levelObjects :: [LevelObject]
  }
  deriving (Show)

data LevelInstance = LevelInstance
  { level :: Level,
    enemies :: [EnemyInstance],
    pickupItems :: [PickupItemInstance]
  }
  deriving (Show)

data TileLayerType = BackgroundTileLayer | ForegroundTileLayer deriving (Show, Eq)

type TileGrid = [(Vec2, Int)]

data TileLayer = TileLayer
  { tileLayerType :: TileLayerType,
    tileGrid :: TileGrid
  }
  deriving (Show)

data EnemyInstance = EnemyInstance
  { enemyType :: EnemyType,
    enemyHealth :: Int,
    enemyPosition :: Vec2
  }
  deriving (Show)

data EnemyType = Regular | Heavy | Fast deriving (Show)

data PickupItemInstance = PickupItemInstance
  { pickupItem :: PickupItem,
    pickupPosition :: Vec2
  }
  deriving (Show)

data PickupItem
  = HealthPotion
  | MaxHealthBoost
  | DamageBoost
  | JumpHeightBoost
  | AmmoPickup WeaponType
  deriving (Show, Eq)

-- Might not be the best name for it, but in our map editor the same name is used.
data LevelObject = LevelObject
  { -- | The player will spawn/respawn in this zone.
    objectName :: String,
    objectPosition :: Vec2,
    objectSize :: Vec2
  }
  deriving (Show)

type CharacterSheets = [CharacterSheet]

data CharacterSheet
  = PlayerCharacterSheet {}
  | InfiltratorCharacterSheet {}

initWorld :: World
initWorld = World (IntroScene 2.5) (Input S.empty [] (0, 0))

initPlayer :: Player
initPlayer = Player 100 100 50 8 empty AssaultRifle (100, 100)

createMenu :: MenuType -> Maybe Scene -> Scene
createMenu m p = MenuScene m p 0

initMainMenu :: Scene
initMainMenu = createMenu MainMenu Nothing

createLevelInstance :: Level -> LevelInstance
createLevelInstance l = LevelInstance l [] []

createGameplay :: Level -> Player -> Scene
createGameplay l p = Gameplay (createLevelInstance l) newPlayer 0
  where
    -- Sets player position to the spawn position in the level.
    newPlayer = p
