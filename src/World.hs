module World (updateWorld) where

import Collision
import Common
import Control.Monad.Random (Rand, RandomGen, evalRand, getRandomR, mkStdGen)
import Data.List (find)
import Data.Map (lookup)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Data.Set (empty, member)
import Enemy
import Graphics.Gloss.Interface.IO.Game
import Input
import Menu
import Model
import Player
import SDL.Font (Font)
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)
import Weapons
import Prelude hiding (lookup)

updateWorld :: [Level] -> Float -> World -> World
updateWorld l d w@(World s i) =
  w
    { -- Update scene
      scene = updateScene l d w,
      -- Clear input event list
      input = i {events = []}
    }

updateScene :: [Level] -> Float -> World -> Scene
-- Intro Scene
updateScene _ d (World (IntroScene dt) i)
  -- Skip intro screen when any key is pressed.
  | not $ null (keys i) = initMainMenu
  -- Keep displaying intro screen while counting down.
  | newDisplayTimer > 0 = IntroScene newDisplayTimer
  | otherwise = initMainMenu
  where
    newDisplayTimer = dt - d
-- Menu Scenes
updateScene l d w@(World s@(MenuScene menuType parentMenu _) _) =
  case menuType of
    -- Main Menu
    MainMenu ->
      let (activatedItem, s) = updateMenuScene 3 d w
       in case activatedItem of
            -- Start the intro level
            Just 0 -> case find ((==) "Intro" . levelName) l of
              Just x -> createGameplay x initPlayer
              Nothing -> trace "No intro level found!" s
            -- Level Select
            Just 1 -> createMenu LevelSelectMenu (Just s)
            -- Quit
            -- I don't want to introduce IO for the whole hierarchy just because of this one function.
            Just 2 -> unsafePerformIO exitSuccess
            -- Default
            _ -> s
    -- Level Select Menu
    LevelSelectMenu ->
      let (activatedItem, s) = updateMenuScene (length l) d w
       in case activatedItem of
            Nothing -> s
            Just x -> createGameplay (l !! x) initPlayer
    -- Gameplay paused menu
    PauseMenu ->
      let (activatedItem, s) = updateMenuScene 2 d w
       in case activatedItem of
            -- Resume
            Just 0 -> fromMaybe s parentMenu
            -- Quit
            Just 1 -> initMainMenu
            -- Default
            _ -> s
    -- End of level menu, should show score etc.
    EndOfLevel gp nextLevel ->
      let itemCount = if isJust nextLevel then 2 else 1
          (activatedItem, s) = updateMenuScene itemCount d w
       in case activatedItem of
            Just i ->
              if i == 0 && isJust nextLevel
                then createGameplay (fromJust nextLevel) (player gp)
                else initMainMenu
            _ -> s
-- Gameplay
-- NOTE: "lens/optics provide a language to do this pattern matching." -- dminuoso
updateScene lvls d' w@(World s@(Gameplay gp) _)
  | MenuBack `elem` events i = createMenu PauseMenu (Just s)
  | playerInLevelEndZone = initEndOfLevel gp nextLevel
  | transCountdown < 0 = initEndOfLevel gp Nothing
  | playerHealth newPlayer <= 0 = Gameplay gp {levelInstance = newLevelInstance, transitionCountdown = transCountdown - d, player = pl {playerHealth = 0}}
  | otherwise = Gameplay gp {levelInstance = newLevelInstance, player = newPlayer, playTime = pt + d}
  where
    i = input w
    pt = playTime gp
    pl = player gp
    (mx, my) = pointer $ input w
    (x, y) = center pl
    (vx, vy) = playerVelocity pl
    state = playerState pl
    lvlInst = levelInstance gp
    lvlEntities = levelEntities lvlInst
    lvlObjs = levelObjects $ level lvlInst
    shootCooldown = playerShootCooldown pl
    selectedWeapon = playerSelectedWeapon pl
    colliders = collisionObjects lvlObjs
    transCountdown = transitionCountdown gp

    d
      | debugMode i = d' * timeMultiplier i
      | otherwise = d'

    newPlayer = (updatePlayer d w gp) {playerShootCooldown = newShootCooldown}

    shouldShootNewBullet :: Bool
    shouldShootNewBullet = isKeyDown i (MouseButton LeftButton) && shootCooldown == 0 && playerHealth newPlayer > 0

    newShootCooldown :: Float
    newShootCooldown
      | shouldShootNewBullet = weaponShootCooldown selectedWeapon
      | otherwise = max 0 $ shootCooldown - d

    playerInLevelEndZone
      | Just nextLevel <- nextLevelObj = intersects nextLevel pl
      | otherwise = False

    nextLevelObj :: Maybe LevelObject
    nextLevelObj = find ((== "LevelEnd") . objectName) lvlObjs

    nextLevel :: Maybe Level
    nextLevel = do
      obj <- nextLevelObj
      nextLevelName <- lookup NextLevel (objectProperties obj)
      find ((== nextLevelName) . levelName) lvls

    newLevelInstance =
      lvlInst
        { levelEntities = filter entityInsideLevel $ mapMaybe updateEntity newLevelEntities,
          levelEnemies = filter ((> 0) . enemyHealth) updatedEnemies,
          levelTimeSinceLastSpawnerTick = newTimeSinceLastSpawnerTick
        }
      where
        newLevelEntities :: [LevelEntity]
        newLevelEntities =
          concat
            [ newBulletList,
              lvlEntities,
              playerDmgList,
              playerDeathList,
              enemyDeathExplosions
            ]
          where
            -- Kinda shitty, but it works.
            playerDmgList = [playerDamageEntity | playerHealth pl > 0 && playerHealth newPlayer < playerHealth pl]
            playerDeathList = [playerDeathEntity | playerHealth pl > 0 && playerHealth newPlayer <= 0]
            newBulletList = [newBullet | shouldShootNewBullet]

            playerDamageEntity = LevelEntity (EffectEntity PlayerDamage 0.15 0) (x, y) (0, 0) (0, 0)
            playerDeathEntity = LevelEntity (EffectEntity PlayerDeath 0.7 0) (x, y) (0, 0) (0, 0)

            newBullet = LevelEntity (Bullet selectedWeapon (x, y) (x, y) bulletTravelDist) (x, y) (0, 0) (dx * speed, dy * speed)
            (totalDistX, totalDistY) = (mx - x, my - y)
            f = sqrt (totalDistX ** 2 + totalDistY ** 2)
            (dx, dy) = (totalDistX / f, totalDistY / f)
            speed = weaponTravelSpeed selectedWeapon

            bulletTravelDist :: Float
            bulletTravelDist = foldr f infinite colliders
              where
                infinite = 1e9 -- Close enough
                bulletLine = ((x, y), (x + dx * infinite, y + dy * infinite))

                f collider closest = case lineIntersectsObject bulletLine collider of
                  Nothing -> closest
                  Just hitPos -> min closest $ euclideanDistance (x, y) hitPos

        entityInsideLevel :: LevelEntity -> Bool
        entityInsideLevel LevelEntity {entityPosition = (x, y)} =
          x > -10 && x < gameWidth + 10 && y > -10 && y < gameHeight + 10

        updateEntity :: LevelEntity -> Maybe LevelEntity
        updateEntity entity@(LevelEntity (EffectEntity fxType totalLifetime lifetime) _ _ _)
          -- Destroy explosion when it is older than the totalLifetime
          | newLifetime > totalLifetime = Nothing
          | otherwise = Just $ entity {entityType = EffectEntity fxType totalLifetime newLifetime}
          where
            newLifetime = lifetime + d
        updateEntity entity@(LevelEntity b@Bullet {bulletType = t} pos@(x, y) size (vx, vy))
          -- If the bullet hits a wall, replace it with an Explosion entity.
          | bulletHitsWall newBullet = Just $ createExplosionAtTravelDist t newBullet
          -- If the bullet hits an enemy, replace it with an Explosion entity.
          | Just hitPos <- bulletHitsEnemy = Just $ createExplosion t hitPos
          -- If the bullet hits nothing then return a new bullet (which has a new position).
          | otherwise = Just newBullet
          where
            newPos = (x + vx * d, y + vy * d)
            newBullet = entity {entityPosition = newPos, entityType = b {bulletPrevPosition = pos}}

            bulletHitsEnemy :: Maybe Vec2
            bulletHitsEnemy = safeHead $ mapMaybe (lineIntersectsObject line) (levelEnemies lvlInst)
              where
                line = (pos, newPos)
        -- Default, do nothing.
        updateEntity x = Just x

        createExplosion :: WeaponType -> Vec2 -> LevelEntity
        createExplosion wpn pos = LevelEntity explosion pos (0, 0) (0, 0)
          where
            explosion
              | wpn == RocketLauncher = EffectEntity DamageExplosion 0.3 0
              | otherwise = EffectEntity BulletImpact 0.15 0

        createExplosionAtTravelDist :: WeaponType -> LevelEntity -> LevelEntity
        createExplosionAtTravelDist wpn (LevelEntity (Bullet _ (ox, oy) _ travelDist) (x, y) size (vx, vy)) =
          createExplosion wpn pos
          where
            -- This could be optimized by using memoization, but it isn't really needed atm.
            s = sqrt (vx ** 2 + vy ** 2) / travelDist
            pos = (ox + vx / s, oy + vy / s)
        createExplosionAtTravelDist _ _ = error "not a bullet"

        bulletHitsWall :: LevelEntity -> Bool
        bulletHitsWall (LevelEntity (Bullet _ (ox, oy) _ travelDist) (x, y) _ _) = dist > travelDist
          where
            dist = sqrt ((x - ox) ** 2 + (y - oy) ** 2)
        bulletHitsWall _ = error "not a bullet"

        shouldSpawnEnemies = levelTimeSinceLastSpawnerTick lvlInst > 1
        newTimeSinceLastSpawnerTick
          | shouldSpawnEnemies = 0
          | otherwise = levelTimeSinceLastSpawnerTick lvlInst + d

        updatedEnemies :: [EnemyInstance]
        updatedEnemies = map (updateEnemy d lvlInst) (levelEnemies lvlInst) ++ spawnedEnemies
          where
            spawners = filter ((==) "EnemySpawner" . objectName) lvlObjs

            spawnedEnemies :: [EnemyInstance]
            spawnedEnemies
              | shouldSpawnEnemies = catMaybes $ evalRand (mapM spawnMaybe spawners) (mkStdGen $ floor pt)
              | otherwise = []

        enemyDeathExplosions :: [LevelEntity]
        enemyDeathExplosions = map f $ filter ((<= 0) . enemyHealth) updatedEnemies
          where
            f e = LevelEntity (EffectEntity EnemyDeath 0.3 0) (center e) (0, 0) (0, 0)
updateScene levels d (World b@(Benchmark w rt) _)
  | rt > 0 =
    b
      { benchmarkWorld = updateWorld levels d w,
        benchmarkRemainingTime = rt - d
      }
  | otherwise = unsafePerformIO (putStrLn "[Benchmark End]" >>= const exitSuccess)
