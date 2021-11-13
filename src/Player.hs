module Player where

import Collision
import Coordinates
import Data.List (find)
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Interface.IO.Game
import Input
import Levels
import Model

updatePlayer :: Float -> World -> GameplayScene -> Player
updatePlayer d w s
  -- Change state to IdleState when the player hasn't moved.
  | newPlayerPosition == (x, y) = newPlBase {playerState = IdleState}
  | otherwise = newPlBase {playerPosition = newPlayerPosition, playerState = MovingState}
  where
    i = input w
    pl = player s
    (mx, my) = pointer $ input w
    (x, y) = playerPosition pl
    (vx, vy) = playerVelocity pl
    state = playerState pl
    lvlInst = levelInstance s
    lvlEntities = levelEntities lvlInst
    lvlObjs = levelObjects $ level lvlInst
    enemies = levelEnemies lvlInst

    -- Constants
    playerSize = size pl

    -- New player base, each update cycle these are the properties that always update, unrelated to player position etc.
    newPlBase = pl {playerHealth = newHp, playerVelocity = newVelocity, playerSelectedWeapon = selectedWeapon}

    newHp
      | touchesInstaDeath = 0
      | otherwise = playerHealth pl - (enemiesDamage + environmentalDamage) * d
      where
        touchesInstaDeath = any (\o -> objectType o == DeathObject && intersects pl o) lvlObjs

        enemiesDamage = sum $ map (enemyDamage . enemyType) enemiesIntersecting
        enemiesIntersecting = filter (intersects pl) enemies

        environmentalDamage = sum $ map environmentDamage environmentalDamageIntersecting
        environmentalDamageIntersecting = filter (\o -> objectType o == DamageObject && intersects pl o) lvlObjs

    selectedWeapon
      | isKeyDown i (Char '1') = AssaultRifle
      | isKeyDown i (Char '2') = PeaShooter
      | isKeyDown i (Char '3') = SniperRifle
      | isKeyDown i (Char '4') = RocketLauncher
      -- Unchanged if no key press
      | otherwise = playerSelectedWeapon pl

    velocityY = tmp + gravity
      where
        tmp
          | onGround && jumpKeyDown = -300
          | canClimb && jumpKeyDown = -100
          | onGround = 0
          | not onGround && not canJumpHigher = 0
          | otherwise = vy

        jumpKeyDown = isKeyDown i (Char 'w') || isKeyDown i (SpecialKey KeySpace)

    velocityX
      | isKeyDown i (Char 'a') = max (-100) vxl
      | isKeyDown i (Char 'd') = min 100 vxr
      | otherwise = 0
      where
        vxl
          | vx > -25 = -25
          | otherwise = - (abs vx ** 1.05)
        vxr
          | vx < 25 = 25
          | otherwise = vx ** 1.05

    newVelocity = (velocityX, velocityY)

    -- WARNING:
    -- We wrote this before we had the `euclideanDistance` and `lineIntersectsObject` functions in Collision.hs
    -- These functions are way better than whatever the f we are doing here. So, should someone in the future be
    -- motivated enough to rewrite the player movement code, then please use those two new functions.
    newPlayerPosition = (newPlayerX, newPlayerY)
      where
        -- Finds the best move we can do. Kinda brute-force, but hey it works.
        newPlayerX =
          fromMaybe x $
            find validMoveX $
              map (\z -> x + (velocityX * z)) [d, d / 2, d / 3, d / 4]

        newPlayerY =
          fromMaybe y $
            find validMoveY $
              map (\z -> y + (velocityY * z)) [d, d / 2, d / 3, d / 4]

    validMove pos size = not $ any (intersects (Box2D pos size)) (collisionObjects lvlObjs)
    validMoveX z = validMove (z, y) playerSize
    validMoveY z = validMove (x, z) playerSize

    -- Don't question it.
    onGroundMagicNumber = 2

    canClimb :: Bool
    canClimb =
      not (validMove (x - 2, y + onGroundMagicNumber) playerSize)
        || not (validMove (x + 2, y + onGroundMagicNumber) playerSize)

    onGround :: Bool
    onGround = not $ validMoveY (y + onGroundMagicNumber)

    canJumpHigher :: Bool
    canJumpHigher = validMoveY (y - 1.2)
