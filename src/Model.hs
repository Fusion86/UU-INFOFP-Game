module Model where

import Data.Map
import Graphics.Gloss

type Assets = Map String Picture

data Player = Player {}

data Scene = Intro {displayTimer :: Float} | Test | MainMenu | Gameplay

data World = World
  { assets :: Assets,
    scene :: Scene
  }
