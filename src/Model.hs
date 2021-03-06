module Model where

import Common
import Data.List (find)
import Data.Map (Map, empty)
import qualified Data.Set as S (Set, empty)
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Interact (Key)
import SDL.Font (Font)
import Text.Printf (printf)

type TileSet = Map Int Picture

type Vec2 = (Float, Float)

type Line = (Vec2, Vec2)

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
        -- | The parent scene, this is the Scene which will be shown when the player presses Esc to go back.
        -- E.g. to unpause the game or to go to a parent menu.
        parentScene :: Maybe Scene,
        -- | Index of the selected menu item. Has to be an Int because the range is not known at compile time.
        selectedItem :: Int
      }
  | Gameplay GameplayScene
  | Benchmark
      { benchmarkWorld :: World,
        benchmarkRemainingTime :: Float
      }
  deriving (Show)

data GameplayScene = GameplayScene
  { -- | An instance of the level which is currently being played.
    levelInstance :: LevelInstance,
    -- | The current player. The player is not stored inside the LevelInstance because the player is supposed to carry over their upgrades, ammo, etc when moving between maps.
    player :: Player,
    -- | Play time in seconds.
    playTime :: Float,
    score :: Int,
    -- | Countdown after the player dies, when this reaches zero we transition to the end of level scene.
    transitionCountdown :: Float
  }
  deriving (Show)

data MenuType = MainMenu | LevelSelectMenu | PauseMenu | EndOfLevel GameplayScene (Maybe Level) Int
  deriving (Show)

data Input = Input
  { -- | A set of the keys currently being pressed.
    keys :: S.Set Key,
    -- | Input events. Each key press corresponds to one event, which also means that multiple key presses produce multiple events.
    events :: [InputEvent],
    -- | The location of the mouse pointer, normalized to our gameWidth and gameHeight.
    pointer :: Vec2,
    debugMode :: Bool,
    timeMultiplier :: Float,
    viewWidth :: Float,
    viewHeight :: Float,
    viewScale :: Float,
    viewIntegerScale :: Bool
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
  | ShootingState
  deriving (Show, Eq)

data Player = Player
  { -- | Current player health.
    playerHealth :: Float,
    -- | Default of 100, can be temporarily increased with upgrades.
    playerMaxHealth :: Float,
    -- | Used as a base for damage calculation, can be increased with upgrades.
    playerDamage :: Float,
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
  | PeaShooter
  | SniperRifle
  | RocketLauncher
  | EnemyWeapon
  deriving (Show, Eq)

data Level = Level
  { levelName :: String,
    levelBackground :: Maybe String,
    levelForeground :: Maybe String,
    levelParallax :: Maybe String,
    layers :: [TileLayer],
    levelObjects :: [LevelObject]
  }
  deriving (Show)

data LevelInstance = LevelInstance
  { level :: Level,
    levelEntities :: [LevelEntity],
    levelEnemies :: [EnemyInstance],
    levelScore :: Int,
    levelTimeSinceLastSpawnerTick :: Float
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
    enemyHealth :: Float,
    enemyPosition :: Vec2,
    enemyVelocity :: Vec2,
    enemyState :: CharacterState,
    enemyShootCooldown :: Float
  }
  deriving (Show)

data EnemyType = CrabEnemy | SunEnemy deriving (Show, Eq)

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
  | Bullet
      { bulletType :: WeaponType,
        bulletStartPosition :: Vec2,
        bulletPrevPosition :: Vec2,
        bulletTravelDistance :: Float
      }
  | EffectEntity EffectEntityType Float Float
  deriving (Show, Eq)

data EffectEntityType
  = GreenBulletImpact
  | BlueBulletImpact
  | RedBulletImpact
  | DamageExplosion
  | PlayerDeath
  | EnemyDeath
  deriving (Show, Eq)

data LevelObjectType
  = CollisionObject
  | EnemyCollisionObject
  | DeathObject
  | DamageObject
  | PlayerSpawnObject
  | EnemySpawnerObject
  | LevelEndObject
  deriving (Show, Eq, Ord)

data LevelObjectProperty
  = SpawnChance
  | NextLevel
  | TypeProperty
  deriving (Show, Eq, Ord)

type LevelObjectProperties = Map LevelObjectProperty String

-- Might not be the best name for it, but in our map editor the same name is used.
data LevelObject = LevelObject
  { objectType :: LevelObjectType,
    objectPosition :: Vec2,
    objectSize :: Vec2,
    objectProperties :: LevelObjectProperties
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
    crabWalkRight :: [Picture],
    sunIdle :: [Picture],
    sunShooting :: Picture
  }
  deriving (Show)

data FxSheet = FxSheet
  { playerBullets :: [Picture],
    greenBulletImpact :: [Picture],
    redBulletImpact :: [Picture],
    blueBulletImpact :: [Picture],
    playerDeath :: [Picture],
    enemyBullet :: Picture,
    smallExplosions :: [Picture],
    explosions :: [Picture],
    fireball :: [Picture]
  }
  deriving (Show)

data OriginPoint = OriginTopLeft | OriginCenter deriving (Show, Eq)

class Object2D a where
  name :: a -> String

  -- | Returns position of this object (origin top-left).
  position :: a -> Vec2

  -- | Returns the center of the object.
  center :: a -> Vec2
  center o = (x + w / 2, y + h / 2)
    where
      (x, y) = position o
      (w, h) = size o

  size :: a -> Vec2

  intersects :: Object2D b => a -> b -> Bool
  intersects l r = outsideWorld || intersects'
    where
      (x1, y1) = position l
      (w1, h1) = size l
      (x2, y2) = position r
      (w2, h2) = size r

      intersects' =
        x1 < x2 + w2
          && x1 + w1 > x2
          && y1 < y2 + h2
          && h1 + y1 > y2

      -- Hacky way to make invisible walls around the world.
      -- This ensures that an object can never fall through the world.
      outsideWorld = x1 < 0 || y1 < 0 || x1 + w1 > gameWidth || y1 + h1 > gameHeight

-- | Used when checking for collision between a theoretical bounding box.
-- For example: to test whether a new player position is valid. In that case
-- the player is not yet at a new position, but we want to test whether they could be.
-- Using a Box2D we can use the 'intersects' function to check whether this position is valid.
data Box2D = Box2D Vec2 Vec2 deriving (Show)

instance Object2D Player where
  name = const "Player"
  position = playerPosition
  size = const (12, 16)

instance Object2D LevelObject where
  name = show . objectType
  position = objectPosition
  size = objectSize

instance Object2D LevelEntity where
  name (LevelEntity (Bullet _ start prev _) _ _ _) = "Bullet " ++ printVec2 start ++ " " ++ printVec2 prev
  name e = "Entity " ++ show (entityType e)
  position = entityPosition
  size = entitySize

instance Object2D EnemyInstance where
  name a = "Enemy " ++ show (enemyType a) ++ " pos: " ++ printVec2 (enemyPosition a) ++ " hp: " ++ printf "%0.2f" (enemyHealth a)
  position = enemyPosition
  size = enemySize . enemyType

instance Object2D Box2D where
  name = const "Box2D"
  position (Box2D a _) = a
  size (Box2D _ b) = b

initWorld :: World
initWorld = World (IntroScene 2.5) initInput

initInput :: Input
initInput = Input S.empty [] (0, 0) False 1 gameWidth gameHeight 1 True

initPlayer :: Player
initPlayer = Player 100 100 50 8 empty AssaultRifle 0 (0, 0) (0, 0) IdleState

createMenu :: MenuType -> Maybe Scene -> Scene
createMenu m p = MenuScene m p 0

initMainMenu :: Scene
initMainMenu = createMenu MainMenu Nothing

initEndOfLevel :: GameplayScene -> Maybe Level -> Int -> Scene
initEndOfLevel gp nextLevel score = createMenu (EndOfLevel gp nextLevel score) Nothing

printVec2 :: Vec2 -> String
printVec2 (x, y) = printf "%0.2f" x ++ ", " ++ printf "%0.2f" y

enemySpeed :: EnemyType -> Float
enemySpeed CrabEnemy = 50
enemySpeed SunEnemy = 25

enemySize :: EnemyType -> Vec2
enemySize CrabEnemy = (14, 14)
enemySize SunEnemy = (16, 16)

enemyDamage :: EnemyType -> Float
enemyDamage = const 100

enemyCanShoot :: EnemyType -> Bool
enemyCanShoot SunEnemy = True
enemyCanShoot _ = False

gravity :: Float
gravity = 10

gameWidth :: Float
gameWidth = 576 -- 8 * 72

gameHeight :: Float
gameHeight = 336 -- 8 * 42

environmentDamage :: LevelObject -> Float
environmentDamage = const 100
