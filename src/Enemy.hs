module Enemy where

import Collision
import Control.Monad.Random (MonadRandom (getRandomR), Rand, RandomGen)
import qualified Control.Monad.Random as Rng (uniform)
import Data.Functor ((<&>))
import Data.List (find)
import Data.Map (lookup)
import Data.Maybe
import Model
import Weapons
import Prelude hiding (lookup)

spawnEnemyMaybe :: RandomGen g => LevelObject -> Rand g (Maybe EnemyInstance)
spawnEnemyMaybe x = do
  -- This is our RandomGen monad
  rng <- getRandomR (0, 100)
  direction <- Rng.uniform [(100, 0), (0, 100)]
  return
    ( -- This is our Maybe monad
      do
        -- Chance is an optional property. If it is missing then just abort (aka return Nothing).
        chance <- lookup SpawnChance (objectProperties x)
        enemy <- spawnEnemy x
        if (read chance :: Int) > rng
          then return $ enemy {enemyVelocity = direction}
          else Nothing
    )

spawnEnemy :: LevelObject -> Maybe EnemyInstance
spawnEnemy x = do
  enemyTypeStr <- lookup TypeProperty (objectProperties x)
  enemyType <- parseEnemyType enemyTypeStr
  return $ EnemyInstance enemyType 100 (objectPosition x) (0, 100) IdleState 2

parseEnemyType :: String -> Maybe EnemyType
parseEnemyType "Crab" = Just CrabEnemy
parseEnemyType "Sun" = Just SunEnemy
parseEnemyType _ = Nothing

updateEnemy :: RandomGen g => Float -> LevelInstance -> EnemyInstance -> Rand g EnemyInstance
updateEnemy d lvlInst enemy = do
  shouldShoot <-
    if enemyCanShoot (enemyType enemy) && enemyShootCooldown enemy == 0
      then getRandomR (0 :: Int, 100) <&> (> 60)
      else return False

  let shootCooldown =
        if shouldShoot
          then weaponShootCooldown EnemyWeapon
          else max 0 (enemyShootCooldown enemy - d)

  let newEnemy =
        enemy
          { enemyPosition = newPosition,
            enemyVelocity = newVelocity,
            enemyHealth = newHp,
            enemyShootCooldown = shootCooldown
          }
  return
    ( if shouldShoot
        then newEnemy {enemyState = ShootingState}
        else newEnemy {enemyState = IdleState}
    )
  where
    lvlEntities = levelEntities lvlInst
    lvlObjs = levelObjects $ level lvlInst
    instaDeathObjects = filter (\o -> objectType o == DeathObject) lvlObjs

    (x, y) = enemyPosition enemy
    (vx, vy) = enemyVelocity enemy
    hp = enemyHealth enemy
    size@(w, h) = enemySize (enemyType enemy)

    newHp
      | any (intersects enemy) instaDeathObjects = 0
      | Just bullet <- enemyHitByBullet = hp - weaponDamage' bullet
      | otherwise = hp - (environmentalDamage + explosionDamage) * d
      where
        enemyHitByBullet = find ((isJust . (`lineIntersectsObject` enemy)) . getBulletHitboxRay d) bullets
        bullets = filter isBullet lvlEntities
          where
            -- Shitty filter
            isBullet (LevelEntity Bullet {} _ _ _) = True
            isBullet _ = False

        environmentalDamage = sum $ map environmentDamage environmentalDamageIntersecting
        environmentalDamageIntersecting = filter (\o -> objectType o == DamageObject && intersects enemy o) lvlObjs

    explosionDamage :: Float
    explosionDamage = sum $ map fst (explosionsHit lvlEntities enemy)

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
