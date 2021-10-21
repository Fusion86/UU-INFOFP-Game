module Collision where

import Model

intersects :: Vec2 -> Vec2 -> LevelObject -> Bool
intersects pos@(x1, y1) (w1, h1) (LevelObject _ (x2, y2) (w2, h2)) =
  x1 < x2 + w2
    && x1 + w1 > x2
    && y1 < y2 + h2
    && h1 + y1 > y2
