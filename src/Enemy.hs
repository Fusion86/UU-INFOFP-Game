module Enemy where

import Collision
import Control.Monad.Random (MonadRandom (getRandomR), Rand, RandomGen)
import qualified Control.Monad.Random as Rng (uniform)
import Data.List (find)
import Data.Maybe
import Model
import Weapons
import Prelude hiding (lookup)
import Data.Map (lookup)

spawnMaybe :: RandomGen g => LevelObject -> Rand g (Maybe EnemyInstance)
spawnMaybe x = do
  -- This is our RandomGen monad
  rng <- getRandomR (0, 100)
  direction <- Rng.uniform [(100, 0), (0, 100)]
  return
    ( -- This is our Maybe monad
      do
        -- Chance is an optional property. If it is missing then just abort (aka return Nothing).
        chance <- lookup SpawnChance (objectProperties x)
        if (read chance :: Int) > rng
          then return $ EnemyInstance CrabEnemy 100 (objectPosition x) direction IdleState
          else Nothing
    )

updateEnemy :: Float -> LevelInstance -> EnemyInstance -> EnemyInstance
updateEnemy d lvlInst enemy = enemy {enemyPosition = newPosition, enemyVelocity = newVelocity, enemyHealth = newHp}
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
      -- TODO: This uses hardcoded damage values.
      -- example: | Just bullet <- enemyHitByBullet = hp - (weaponDamage bullet)
      -- ... ^ this won't work because the bullet type is trash (just like this language).
      | Just bullet <- enemyHitByBullet = hp - dmg bullet
      -- if hit by explosion from rocket launcher
      | explosionDamage > 0 = hp - explosionDamage
      | otherwise = hp
      where
        dmg (LevelEntity Bullet {bulletType = wp} _ _ _) = weaponDamage wp
        dmg _ = 0

    enemyHitByBullet = find ((isJust . (`lineIntersectsObject` enemy)) . getBulletHitboxRay d) bullets
    bullets = filter isBullet lvlEntities
      where
        -- Shitty filter
        isBullet (LevelEntity Bullet {} _ _ _) = True
        isBullet _ = False

    explosionDamage :: Float
    explosionDamage = sum $ map fst explosionsHit

    -- Returns a list with the explosions that hit the enemy.
    -- fst = damage, snd = position of the explosion.
    -- `snd` could be used to implement knock-back.
    explosionsHit = mapMaybe f explosions
      where
        blastDamage = 200
        blastRadius = 50

        f :: LevelEntity -> Maybe (Float, Vec2)
        f e
          | dmg > 0 = Just (dmg, position e)
          | otherwise = Nothing
          where
            dmg = max 0 $ (1 - (dist / blastRadius)) * blastDamage
            dist = euclideanDistance (position e) (position enemy)

    explosions = filter isExplosion lvlEntities
      where
        isExplosion (LevelEntity (EffectEntity DamageExplosion _ 0) _ _ _) = True
        isExplosion _ = False

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
