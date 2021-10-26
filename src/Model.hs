module Model where

import Data.Map (Map, empty)
import qualified Data.Set as S (Set, empty)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Interact (Key)
import SDL.Font (Font)

type TileSet = Map Int Picture

type Vec2 = (Float, Float)

data Assets = Assets
  { images :: Map String Picture,
    playerSheet :: PlayerCharacterSheet,
    fxSheet :: FxSheet,
    enemyCharacterSheet :: EnemyCharacterSheet
  }
  deriving (Show)

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

data CharacterState
  = MovingState
  | IdleState
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
    playerShootCooldown :: Float,
    -- | The player's position within the current active level instance.
    playerPosition :: Vec2,
    playerVelocity :: Vec2,
    playerState :: CharacterState
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
    levelEntities :: [LevelEntity],
    levelEnemies :: [EnemyInstance]
    -- pickupItems :: [PickupItemInstance]
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
    enemyPosition :: Vec2,
    enemyVelocity :: Vec2,
    enemyState :: CharacterState
  }
  deriving (Show)

data EnemyType = CrabEnemy | Heavy | Fast deriving (Show, Eq)

-- data PickupItemInstance = PickupItemInstance
--   { pickupItem :: PickupItem,
--     pickupPosition :: Vec2
--   }
--   deriving (Show)

data LevelEntity = LevelEntity
  { entityType :: EntityType,
    entityPosition :: Vec2,
    entitySize :: Vec2,
    entityVelocity :: Vec2
  }
  deriving (Show)

data EntityType
  = HealthPotion
  | MaxHealthBoost
  | DamageBoost
  | JumpHeightBoost
  | AmmoPickup WeaponType
  | Bullet WeaponType
  | ExplosionEntity Float Float
  deriving (Show, Eq)

-- Might not be the best name for it, but in our map editor the same name is used.
data LevelObject = LevelObject
  { -- | The player will spawn/respawn in this zone.
    objectName :: String,
    objectPosition :: Vec2,
    objectSize :: Vec2
  }
  deriving (Show)

data PlayerCharacterSheet = PlayerCharacterSheet
  { playerIdleRight :: Picture,
    playerIdleLeft :: Picture,
    playerWalkRight :: [Picture],
    playerWalkLeft :: [Picture]
  }
  deriving (Show)

data EnemyCharacterSheet = EnemyCharacterSheet
  { crabIdle :: Picture,
    crabWalkLeft :: [Picture],
    crabWalkRight :: [Picture]
  }
  deriving (Show)

data FxSheet = FxSheet
  { playerBulletImpact :: [Picture]
  }
  deriving (Show)

initWorld :: World
initWorld = World (IntroScene 2.5) (Input S.empty [] (0, 0))

initPlayer :: Player
initPlayer = Player 100 100 50 8 empty AssaultRifle 0 (100, 100) (0, 0) IdleState

createMenu :: MenuType -> Maybe Scene -> Scene
createMenu m p = MenuScene m p 0

initMainMenu :: Scene
initMainMenu = createMenu MainMenu Nothing

createLevelInstance :: Level -> LevelInstance
createLevelInstance l =
  LevelInstance l [] [EnemyInstance CrabEnemy 100 (30, 30) (100, 0) IdleState]

createGameplay :: Level -> Player -> Scene
createGameplay l p = Gameplay (createLevelInstance l) newPlayer 0
  where
    -- TODO: Set player position to the spawn position in the level.
    newPlayer = p

weaponShootCooldown :: WeaponType -> Float
weaponShootCooldown _ = 0.1

enemySpeed :: EnemyType -> Float
enemySpeed _ = 50

gravity :: Float
gravity = 10
