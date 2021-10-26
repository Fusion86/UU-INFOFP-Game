module World (updateWorld) where

import Assets
import Collision
import Common
import Coordinates
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (empty, member)
import Graphics.Gloss.Interface.IO.Game
import Input
import Levels
import Menu
import Model
import Player
import Rendering
import SDL.Font (Font)
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)

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
            -- Start game, currently always starts the first level.
            Just 0 -> createGameplay (head l) initPlayer
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
    EndOfLevel -> s
-- Gameplay
-- NOTE: "lens/optics provide a language to do this pattern matching." -- dminuoso
updateScene _ d w@(World s@Gameplay {} _)
  | MenuBack `elem` events i = createMenu PauseMenu (Just s)
  | otherwise = s {levelInstance = newLevelInstance, player = newPlayer, playTime = pt + d}
  where
    i = input w
    pt = playTime $ scene w
    pl = player $ scene w
    (mx, my) = pointer $ input w
    (x, y) = playerPosition pl
    (vx, vy) = playerVelocity pl
    state = playerState pl
    lvlInst = levelInstance s
    lvlEntities = levelEntities lvlInst
    lvlObjs = levelObjects $ level $ levelInstance s
    shootCooldown = playerShootCooldown pl
    selectedWeapon = playerSelectedWeapon pl
    colliders = collisionObjects lvlObjs

    newPlayer = (updatePlayer d w s) {playerShootCooldown = newShootCooldown}

    shouldShootNewBullet :: Bool
    shouldShootNewBullet = isKeyDown i (MouseButton LeftButton) && shootCooldown == 0

    newShootCooldown :: Float
    newShootCooldown
      | shouldShootNewBullet = weaponShootCooldown selectedWeapon
      | otherwise = max 0 $ shootCooldown - d

    newLevelInstance = lvlInst {levelEntities = mapMaybe updateEntity newLevelEntities, levelEnemies = mapMaybe updateEnemy newLevelEnemies}
      where
        newLevelEntities :: [LevelEntity]
        newLevelEntities
          | shouldShootNewBullet = newBullet : lvlEntities
          | otherwise = lvlEntities
          where
            newBullet = LevelEntity (Bullet AssaultRifle) (x, y) (0, 0) (5 * dx', 5 * dy')
            (dx, dy) = (mx - x, my - y)
            f = sqrt (dx ** 2 + dy ** 2)
            (dx', dy') = (dx / f, dy / f)

        updateEntity :: LevelEntity -> Maybe LevelEntity
        updateEntity entity@(LevelEntity (ExplosionEntity totalLifetime lifetime) _ _ _)
          -- Destroy bullet when it is older than the totalLifetime
          | newLifetime > totalLifetime = Nothing
          | otherwise = Just $ entity {entityType = ExplosionEntity totalLifetime newLifetime}
          where
            newLifetime = lifetime + d
        updateEntity entity@(LevelEntity t pos@(x, y) size (vx, vy))
          -- If the bullet hits a wall, replace it with an Explosion entity.
          | bulletHitsWall = Just $ LevelEntity (ExplosionEntity 0.15 0) pos size (0, 0)
          | otherwise = Just entity {entityPosition = newPos}
          where
            newPos = (x + vx, y + vy)

            bulletHitsWall :: Bool
            bulletHitsWall = doesCollide colliders newPos size

        newLevelEnemies :: [EnemyInstance]
        newLevelEnemies = levelEnemies lvlInst

        updateEnemy :: EnemyInstance -> Maybe EnemyInstance
        updateEnemy enemy = Just $ enemy {enemyPosition = newPosition, enemyVelocity = newVelocity}
          where
            (x, y) = enemyPosition enemy
            (vx, vy) = enemyVelocity enemy
            size@(w, h) = (14, 14)

            speed = enemySpeed (enemyType enemy)
            acceleration = speed / 4
            newVelocity = (velocityX, velocityY)

            velocityY = tmp + gravity
              where
                tmp
                  | onGround = 0
                  | otherwise = vy

            velocityX
              | onGround && not (validMoveX (x + 10)) = vx - acceleration
              | onGround && not (validMoveX (x - 10)) = vx + acceleration
              | vx > 0 = min speed $ vx + acceleration
              | otherwise = max (-speed) $ vx - acceleration

            newPosition = (newX, newY)
              where
                newX =
                  fromMaybe x $
                    find validMoveX $
                      map (\z -> x + (velocityX * z)) [d, d / 2, d / 3, d / 4]

                newY =
                  fromMaybe y $
                    find validMoveY $
                      map (\z -> y + (velocityY * z)) [d, d / 2, d / 3, d / 4]

            validMove pos@(x, y) size = not $ doesCollide (collisionObjects lvlObjs) (x - 7, y - 7) size
            validMoveX z = validMove (z, y) size
            validMoveY z = validMove (x, z) size

            -- Don't question it.
            onGroundMagicNumber = 2

            onGround :: Bool
            onGround = not $ validMoveY (y + onGroundMagicNumber)
