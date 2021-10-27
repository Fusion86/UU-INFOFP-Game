module Collision where

import Coordinates
import Model

intersects :: Vec2 -> Vec2 -> LevelObject -> Bool
intersects pos@(x1, y1) (w1, h1) (LevelObject _ (x2, y2) (w2, h2) _) =
  x1 < x2 + w2
    && x1 + w1 > x2
    && y1 < y2 + h2
    && h1 + y1 > y2

collisionObjects :: [LevelObject] -> [LevelObject]
collisionObjects = filter ((==) "Collision" . objectName)

enemyCollisionObjects :: [LevelObject] -> [LevelObject]
enemyCollisionObjects = filter (f . objectName)
  where
    f x = x == "Collision" || x == "EnemyCollision"

doesCollide :: [LevelObject] -> Vec2 -> Vec2 -> Bool
doesCollide colliders pos@(x, y) size@(w, h)
  | x < 0 || y < 0 || x + w > worldWidth || y + h > worldHeight = True
  | otherwise = any (intersects pos size) colliders
