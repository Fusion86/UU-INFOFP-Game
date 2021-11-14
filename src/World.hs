module World where

import Collision
import Common
import Control.Monad.Random (Rand, RandomGen, evalRand, getRandomR, mkStdGen)
import Data.List (find)
import Data.Map (lookup)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe, maybeToList)
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

createLevelInstance :: Level -> LevelInstance
createLevelInstance l = LevelInstance l [] enemies 0 0
  where
    enemies = mapMaybe spawnEnemy $ filter ((==) EnemySpawnerObject . objectType) (levelObjects l)

createGameplay :: Level -> Player -> Float -> Int -> Scene
createGameplay l p pt score = Gameplay $ GameplayScene (createLevelInstance l) newPlayer pt score 2
  where
    newPlayer
      | Just spawnPos <- playerSpawnPos = p {playerPosition = spawnPos}
      | otherwise =
        trace "No PlayerSpawn defined, using default values of (100,100)" $
          p {playerPosition = (100, 100)}

    playerSpawnPos = do
      spawnObj <- find ((==) PlayerSpawnObject . objectType) (levelObjects l)
      return $ position spawnObj

createBenchmark :: Level -> Scene
createBenchmark l = Benchmark (World (createGameplay l dummyPlayer 0 0) initInput) 30
  where
    dummyPlayer = initPlayer

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
              Just x -> createGameplay x initPlayer 0 0
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
            Just x -> createGameplay (l !! x) initPlayer 0 0
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
    EndOfLevel gp nextLevel score ->
      let itemCount = if isJust nextLevel then 2 else 1
          (activatedItem, s) = updateMenuScene itemCount d w
       in case activatedItem of
            Just i ->
              if i == 0 && isJust nextLevel
                then createGameplay (fromJust nextLevel) (player gp) (playTime gp) score
                else initMainMenu
            _ -> s
-- Gameplay
-- NOTE: "lens/optics provide a language to do this pattern matching." -- dminuoso
updateScene lvls d' w@(World s@(Gameplay gp) _)
  | MenuBack `elem` events i = createMenu PauseMenu (Just s)
  | playerInLevelEndZone = initEndOfLevel gp nextLevel (score gp + levelScore newLevelInstance)
  | transCountdown < 0 = initEndOfLevel gp Nothing (score gp + levelScore newLevelInstance)
  | playerHealth newPlayer <= 0 = Gameplay gp {levelInstance = newLevelInstance, transitionCountdown = transCountdown - d, player = pl {playerHealth = 0}, playTime = pt + d}
  | otherwise = Gameplay gp {levelInstance = newLevelInstance, player = newPlayer, playTime = pt + d}
  where
    i = input w
    pt = playTime gp
    pl = player gp
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
    nextLevelObj = find ((== LevelEndObject) . objectType) lvlObjs

    nextLevel :: Maybe Level
    nextLevel = do
      obj <- nextLevelObj
      nextLevelName <- lookup NextLevel (objectProperties obj)
      find ((== nextLevelName) . levelName) lvls

    newLevelInstance =
      lvlInst
        { levelEntities = filter entityInsideLevel $ mapMaybe updateEntity newLevelEntities,
          levelEnemies = filter ((> 0) . enemyHealth) updatedEnemies,
          levelTimeSinceLastSpawnerTick = newTimeSinceLastSpawnerTick,
          levelScore = levelScore lvlInst + length enemiesThatWillDie
        }
      where
        newLevelEntities :: [LevelEntity]
        newLevelEntities =
          concat
            [ newBulletList,
              newEnemyBulletList,
              lvlEntities,
              playerDmgList,
              playerDeathList,
              enemyDeathExplosions
            ]
          where
            -- Kinda shitty, but it works.
            playerDmgList = [playerDamageEntity | playerHealth pl > 0 && playerHealth newPlayer < playerHealth pl]
            playerDeathList = [playerDeathEntity | playerHealth pl > 0 && playerHealth newPlayer <= 0]
            newBulletList = [x | shouldShootNewBullet, x <- maybeToList newBullet]

            playerDamageEntity = LevelEntity (EffectEntity RedBulletImpact 0.15 0) (x, y) (0, 0) (0, 0)
            playerDeathEntity = LevelEntity (EffectEntity PlayerDeath 0.7 0) (x, y) (0, 0) (0, 0)

            newBullet = shootBulletToPosition colliders selectedWeapon (x, y) (pointer $ input w) False

            newEnemyBulletList = mapMaybe f $ filter ((== ShootingState) . enemyState) updatedEnemies
              where
                f :: EnemyInstance -> Maybe LevelEntity
                f e = shootBulletToPosition colliders EnemyWeapon (center e) (x, y) True

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
          -- If the bullet hits an enemy, replace it with a hit marker.
          | Just hitPos <- bulletHitsEnemy = Just $ createExplosion t hitPos
          -- If the bullet hits us, just delete the bullet.
          | Just hitPos <- bulletHitsPlayer = Nothing
          -- If the bullet hits nothing then return a new bullet (which has a new position).
          | otherwise = Just newBullet
          where
            newPos = (x + vx * d, y + vy * d)
            newBullet = entity {entityPosition = newPos, entityType = b {bulletPrevPosition = pos}}

            bulletHitsEnemy :: Maybe Vec2
            bulletHitsEnemy
              | t == EnemyWeapon = Nothing
              | otherwise = safeHead $ mapMaybe (lineIntersectsObject (pos, newPos)) (levelEnemies lvlInst)

            bulletHitsPlayer :: Maybe Vec2
            bulletHitsPlayer
              | t /= EnemyWeapon = Nothing
              | otherwise = lineIntersectsObject (pos, newPos) pl
        -- Default, do nothing.
        updateEntity x = Just x

        createExplosion :: WeaponType -> Vec2 -> LevelEntity
        createExplosion wpn pos = LevelEntity explosion pos (0, 0) (0, 0)
          where
            explosion
              | wpn == RocketLauncher = EffectEntity DamageExplosion 0.3 0
              | wpn == EnemyWeapon = EffectEntity RedBulletImpact 0.15 0
              | otherwise = EffectEntity GreenBulletImpact 0.15 0

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
        updatedEnemies = evalRand (mapM (updateEnemy d lvlInst) (levelEnemies lvlInst)) randGen ++ spawnedEnemies
          where
            randGen = mkStdGen $ floor pt
            spawners = filter ((==) EnemySpawnerObject . objectType) lvlObjs

            spawnedEnemies :: [EnemyInstance]
            spawnedEnemies
              | shouldSpawnEnemies = catMaybes $ evalRand (mapM spawnEnemyMaybe spawners) randGen
              | otherwise = []

        enemiesThatWillDie = filter ((<= 0) . enemyHealth) updatedEnemies

        enemyDeathExplosions :: [LevelEntity]
        enemyDeathExplosions = map f enemiesThatWillDie
          where
            f e = LevelEntity (EffectEntity EnemyDeath 0.3 0) (center e) (0, 0) (0, 0)
updateScene levels d (World b@(Benchmark w rt) _)
  | rt > 0 =
    b
      { benchmarkWorld = updateWorld levels d w,
        benchmarkRemainingTime = rt - d
      }
  | otherwise = unsafePerformIO (putStrLn "[Benchmark End]" >>= const exitSuccess)
