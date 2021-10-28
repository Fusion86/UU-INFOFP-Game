module Collision where

import Coordinates
import Model

collisionObjects :: [LevelObject] -> [LevelObject]
collisionObjects = filter ((==) "Collision" . objectName)

enemyCollisionObjects :: [LevelObject] -> [LevelObject]
enemyCollisionObjects = filter (f . objectName)
  where
    f x = x == "Collision" || x == "EnemyCollision"
