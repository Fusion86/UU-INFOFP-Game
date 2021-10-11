module Model where

import Data.Map (Map, empty)
import qualified Data.Set as S (Set, empty)
import GHC.Enum (Enum)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Interact (Key)
import SDL.Font (Font)

type Assets = Map String Picture

type TileSet = Map Int Picture

data World = World
  { scene :: Scene,
    input :: Input
  }
  deriving (Show)

data WeaponType
  = AssaultRifle
  | RocketLauncher
  | Shotgun
  deriving (Show)

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
    playerPosition :: (Float, Float)
  }
  deriving (Show)

data MenuType = MainMenu | LevelSelectMenu | PauseMenu
  deriving (Show, Eq)

data Scene
  = IntroScene {displayTimer :: Float}
  | MenuScene
      { -- | Menu type.
        menuType :: MenuType,
        parentScene :: Maybe Scene,
        -- | Index of the selected menu item.
        selectedItem :: Int
      }
  | Gameplay
      { levelInstance :: LevelInstance,
        player :: Player,
        playTime :: Float
      }
  | EndOfLevelScene
  deriving (Show)

data LevelInstance = LevelInstance
  { level :: Level,
    enemies :: [EnemyInstance],
    pickupItems :: [PickupItemInstance]
  }
  deriving (Show)

data EnemyType = Regular | Heavy | Fast deriving (Show)

data EnemyInstance = EnemyInstance
  { enemyType :: EnemyType,
    enemyHealth :: Int,
    enemyPosition :: (Float, Float)
  }
  deriving (Show)

data PickupItemInstance = PickupItemInstance
  { pickupItem :: PickupItem,
    pickupPosition :: (Float, Float)
  }
  deriving (Show)

data PickupItem
  = HealthPotion
  | MaxHealthBoost
  | DamageBoost
  | JumpHeightBoost
  | AmmoPickup WeaponType
  deriving (Show)

data InputEvent
  = MenuDown
  | MenuUp
  | MenuEnter
  | MenuBack
  deriving (Show, Eq)

data Input = Input
  { keys :: S.Set Key,
    events :: [InputEvent],
    pointer :: (Float, Float)
  }
  deriving (Show)

data TileLayerType
  = SolidTileLayer
  | BackgroundTileLayer
  | ForegroundTileLayer
  deriving (Show, Eq)

data TileLayer = TileLayer
  { tileLayerType :: TileLayerType,
    tiles :: [Int]
  }
  deriving (Show)

data Level = Level
  { levelName :: String,
    levelBackground :: Maybe String,
    layers :: [TileLayer],
    levelObjects :: [LevelObject]
  }
  deriving (Show)

data PlayerSpawnZoneType = StartSpawn | EndSpawn deriving (Show)

data LevelObject
  = PlayerSpawn
      { playerSpawnType :: PlayerSpawnZoneType,
        playerSpawnPosition :: (Float, Float)
      }
  | EnemySpawn
      { enemySpawnZone :: (Float, Float, Float, Float)
      }
  | LevelEnd
      { levelEndZone :: (Float, Float, Float, Float),
        levelEndNextLevel :: String
      }
  | DeathZone
      {
      }
  deriving (Show)

initWorld :: World
initWorld = World (IntroScene 2.5) (Input S.empty [] (0, 0))

initPlayer :: Player
initPlayer = Player 100 100 50 8 empty AssaultRifle (0, 0)

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
