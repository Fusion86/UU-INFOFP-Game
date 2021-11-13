module Collision where

import Coordinates
import Data.Maybe
import Model

collisionObjects :: [LevelObject] -> [LevelObject]
collisionObjects = filter ((==) CollisionObject . objectType)

enemyCollisionObjects :: [LevelObject] -> [LevelObject]
enemyCollisionObjects = filter (f . objectType)
  where
    f x = x == CollisionObject || x == EnemyCollisionObject

euclideanDistance :: Vec2 -> Vec2 -> Float
euclideanDistance (x1, y1) (x2, y2) = sqrt ((x1 - x2) ** 2 + (y1 - y2) ** 2)

lineIntersectsObject :: Object2D a => Line -> a -> Maybe Vec2
lineIntersectsObject l@((x, y), _) box
  | [a] <- intersections = Just a
  | [a, b] <- intersections = Just $ helper a b
  | otherwise = Nothing
  where
    -- Returns the Vec2 which is the closest to the start of the line.
    helper :: Vec2 -> Vec2 -> Vec2
    helper a@(ax, ay) b@(bx, by)
      | distA <= distB = a
      | otherwise = b
      where
        (vecAX, vecAY) = (ax - x, ay - y)
        (vecBX, vecBY) = (bx - x, by - y)

        distA = sqrt (vecAX ** 2 + vecAY ** 2)
        distB = sqrt (vecBX ** 2 + vecBY ** 2)

    (rx, ry) = position box
    (rw, rh) = size box
    left = ((rx, ry), (rx, ry + rh))
    right = ((rx + rw, ry), (rx + rw, ry + rh))
    top = ((rx, ry), (rx + rw, ry))
    bottom = ((rx, ry + rh), (rx + rw, ry + rh))
    intersections = mapMaybe (linesIntersect l) [left, right, top, bottom]

-- | Check whether two lines intersect, and if they do return the position of where they intersect.
-- Taken from http://www.jeffreythompson.org/collision-detection/line-rect.php
linesIntersect :: Line -> Line -> Maybe Vec2
linesIntersect ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
  | uA >= 0 && uA <= 1 && uB >= 0 && uB <= 1 = Just (x1 + (uA * (x2 - x1)), y1 + (uA * (y2 - y1)))
  | otherwise = Nothing
  where
    uA = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / ((y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1))
    uB = ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)) / ((y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1))

getBulletHitboxRay :: Float -> LevelEntity -> Line
getBulletHitboxRay d entity@(LevelEntity t@Bullet {} pos@(x, y) size (vx, vy)) =
  (pos, (x + vx * d, y + vy * d))
getBulletHitboxRay _ _ = error "not a bullet"
