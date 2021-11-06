module World (updateWorld) where

import Assets
import Collision
import Common
import Coordinates
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
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
import Weapons

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
updateScene _ d' w@(World s@Gameplay {} _)
  | MenuBack `elem` events i = createMenu PauseMenu (Just s)
  | otherwise = s {levelInstance = newLevelInstance, player = newPlayer, playTime = pt + d}
  where
    i = input w
    pt = playTime $ scene w
    pl = player $ scene w
    (mx, my) = pointer $ input w
    (x, y) = center pl
    (vx, vy) = playerVelocity pl
    state = playerState pl
    lvlInst = levelInstance s
    lvlEntities = levelEntities lvlInst
    lvlObjs = levelObjects $ level $ levelInstance s
    shootCooldown = playerShootCooldown pl
    selectedWeapon = playerSelectedWeapon pl
    colliders = collisionObjects lvlObjs

    d
      | debugMode i = d' * timeMultiplier i
      | otherwise = d'

    newPlayer = (updatePlayer d w s) {playerShootCooldown = newShootCooldown}

    shouldShootNewBullet :: Bool
    shouldShootNewBullet = isKeyDown i (MouseButton LeftButton) && shootCooldown == 0

    newShootCooldown :: Float
    newShootCooldown
      | shouldShootNewBullet = weaponShootCooldown selectedWeapon
      | otherwise = max 0 $ shootCooldown - d

    newLevelInstance =
      lvlInst
        { levelEntities = filter entityInsideLevel $ mapMaybe updateEntity newLevelEntities,
          levelEnemies = mapMaybe updateEnemy newLevelEnemies
        }
      where
        newLevelEntities :: [LevelEntity]
        newLevelEntities
          | shouldShootNewBullet = newBullet : lvlEntities
          | otherwise = lvlEntities
          where
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
        updateEntity entity@(LevelEntity (ExplosionEntity totalLifetime lifetime) _ _ _)
          -- Destroy explosion when it is older than the totalLifetime
          | newLifetime > totalLifetime = Nothing
          | otherwise = Just $ entity {entityType = ExplosionEntity totalLifetime newLifetime}
          where
            newLifetime = lifetime + d
        updateEntity entity@(LevelEntity t@Bullet {} pos@(x, y) size (vx, vy))
          -- If the bullet hits a wall, replace it with an Explosion entity.
          | bulletHitsWall newBullet = Just $ createExplosionAtTravelDist newBullet
          -- If the bullet hits an enemy, replace it with an Explosion entity.
          | Just hitPos <- bulletHitsEnemy = Just $ createExplosion hitPos
          -- If the bullet hits nothing then return a new bullet (which has a new position).  
          | otherwise = Just newBullet
          where
            newPos = (x + vx * d, y + vy * d)
            newBullet = entity {entityPosition = newPos, entityType = t {bulletPrevPosition = pos}}

            bulletHitsEnemy :: Maybe Vec2
            bulletHitsEnemy = safeHead $ mapMaybe (lineIntersectsObject line) (levelEnemies lvlInst)
              where
                line = (pos, newPos)
        -- Default, do nothing.
        updateEntity x = Just x

        createExplosion :: Vec2 -> LevelEntity
        createExplosion pos = LevelEntity (ExplosionEntity 0.15 0) pos (0, 0) (0, 0)

        createExplosionAtTravelDist :: LevelEntity -> LevelEntity
        createExplosionAtTravelDist (LevelEntity (Bullet _ (ox, oy) _ travelDist) (x, y) size (vx, vy)) =
          createExplosion pos
          where
            -- This could be optimized by using memoization, but it isn't really needed atm.
            s = sqrt (vx ** 2 + vy ** 2) / travelDist
            pos = (ox + vx / s, oy + vy / s)
        createExplosionAtTravelDist _ = error "not a bullet"

        bulletHitsWall :: LevelEntity -> Bool
        bulletHitsWall (LevelEntity (Bullet _ (ox, oy) _ travelDist) (x, y) _ _) = dist > travelDist
          where
            dist = sqrt ((x - ox) ** 2 + (y - oy) ** 2)
        bulletHitsWall _ = error "not a bullet"

        newLevelEnemies :: [EnemyInstance]
        newLevelEnemies = levelEnemies lvlInst

        updateEnemy :: EnemyInstance -> Maybe EnemyInstance
        updateEnemy enemy
          | newHp > 0 = Just $ enemy {enemyPosition = newPosition, enemyVelocity = newVelocity, enemyHealth = newHp}
          | otherwise = Nothing
          where
            (x, y) = enemyPosition enemy
            (vx, vy) = enemyVelocity enemy
            hp = enemyHealth enemy
            size@(w, h) = enemySize (enemyType enemy)

            newHp
              -- TODO: This uses hardcoded damage values.
              -- example: | Just bullet <- enemyHitByBullet = hp - (weaponDamage bullet)
              -- ... ^ this won't work because the bullet type is trash (just like this language).
              | Just bullet <- enemyHitByBullet = hp - dmg bullet
              | otherwise = hp
              where
                dmg (LevelEntity Bullet {bulletType = wp} _ _ _) = weaponDamage wp
                dmg _ = 0

            enemyHitByBullet = find ((isJust . (`lineIntersectsObject` enemy)) . getBulletHitboxRay d) bullets
            bullets = filter f lvlEntities
              where
                -- Shitty filter
                f (LevelEntity Bullet {} _ _ _) = True
                f _ = False

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
              | otherwise = max (- speed) $ vx - acceleration

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

            validMove pos size = not $ any (intersects (Box2D pos size)) (enemyCollisionObjects lvlObjs)
            validMoveX z = validMove (z, y) size
            validMoveY z = validMove (x, z) size

            -- Don't question it.
            onGroundMagicNumber = 2

            onGround :: Bool
            onGround = not $ validMoveY (y + onGroundMagicNumber)

getBulletHitboxRay :: Float -> LevelEntity -> Line
getBulletHitboxRay d entity@(LevelEntity t@Bullet {} pos@(x, y) size (vx, vy)) =
  (pos, (x + vx * d, y + vy * d))
getBulletHitboxRay _ _ = error "not a bullet"
