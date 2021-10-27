module Player where

import Collision
import Coordinates
import Data.List (find)
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Interface.IO.Game
import Input
import Levels
import Model

updatePlayer :: Float -> World -> Scene -> Player
updatePlayer d w s
  -- Change state to IdleState when the player hasn't moved.
  | newPlayerPosition == (x, y) = newPlBase {playerState = IdleState}
  | otherwise = newPlBase {playerPosition = newPlayerPosition, playerState = MovingState}
  where
    i = input w
    pl = player $ scene w
    (mx, my) = pointer $ input w
    (x, y) = playerPosition pl
    (vx, vy) = playerVelocity pl
    state = playerState pl
    lvlInst = levelInstance s
    lvlEntities = levelEntities lvlInst
    lvlObjs = levelObjects $ level lvlInst

    -- New player base, each update cycle these are the properties that always update, unrelated to player position etc.
    newPlBase = pl {playerVelocity = newVelocity, playerSelectedWeapon = selectedWeapon}

    -- Constants
    playerSize = (12, 16)

    selectedWeapon
      | isKeyDown i (Char '1') = AssaultRifle
      | isKeyDown i (Char '2') = PeaShooter
      | isKeyDown i (Char '3') = Shotgun
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

    -- -8 and -4 to move the origin point from the center to the top left.
    -- -8 = playerSize X / 2
    -- -4 = playerSize Y / 2
    validMove pos@(x, y) size = not $ doesCollide (collisionObjects lvlObjs) (x - 6, y - 4) size
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
