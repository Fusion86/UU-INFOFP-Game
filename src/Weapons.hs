module Weapons where

import Collision
import Data.Maybe (mapMaybe)
import Model

weaponShootCooldown :: WeaponType -> Float
weaponShootCooldown AssaultRifle = 0.1
weaponShootCooldown PeaShooter = 0.07
weaponShootCooldown SniperRifle = 0.8
weaponShootCooldown RocketLauncher = 1
weaponShootCooldown EnemyWeapon = 0.4

weaponDamage :: WeaponType -> Float
weaponDamage AssaultRifle = 15
weaponDamage PeaShooter = 9
weaponDamage SniperRifle = 114
weaponDamage RocketLauncher = 0
weaponDamage EnemyWeapon = 10

weaponDamage' :: LevelEntity -> Float
weaponDamage' (LevelEntity Bullet {bulletType = wp} _ _ _) = weaponDamage wp
weaponDamage' _ = 0

weaponTravelSpeed :: WeaponType -> Float
weaponTravelSpeed AssaultRifle = 8 * 715 / 6 -- ak47
weaponTravelSpeed PeaShooter = 8 * 400 / 6 -- mp9
weaponTravelSpeed SniperRifle = 8 * 936 / 4 -- l98 lapua / awp
weaponTravelSpeed RocketLauncher = 8 * 300 / 6 -- rpg-7 flight
weaponTravelSpeed EnemyWeapon = 650

-- Returns a list with the explosions that hit the enemy.
-- fst = damage, snd = position of the explosion.
explosionsHit :: Object2D a => [LevelEntity] -> a -> [(Float, Vec2)]
explosionsHit lvlEntities obj = mapMaybe f (filterExplosions lvlEntities)
  where
    blastDamage = 12000
    blastRadius = 60

    f :: LevelEntity -> Maybe (Float, Vec2)
    f e
      | dmg > 0 = Just (dmg, position e)
      | otherwise = Nothing
      where
        dmg = max 0 $ (1 - (dist / blastRadius)) * blastDamage
        dist = euclideanDistance (position e) (position obj)

filterExplosions :: [LevelEntity] -> [LevelEntity]
filterExplosions = filter isExplosion
  where
    isExplosion (LevelEntity (EffectEntity DamageExplosion _ 0) _ _ _) = True
    isExplosion _ = False

shootBulletToPosition :: [LevelObject] -> WeaponType -> Vec2 -> Vec2 -> Bool -> Maybe LevelEntity
shootBulletToPosition colliders selectedWeapon origin@(x, y) (mx, my) requireLOS
  | requireLOS && (bulletTravelDist + 1) < distToTarget = Nothing
  | otherwise = return $ LevelEntity (Bullet selectedWeapon (x, y) (x, y) bulletTravelDist) (x, y) (0, 0) (dx * speed, dy * speed)
  where
    distToTarget = euclideanDistance (x, y) (mx, my)

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
