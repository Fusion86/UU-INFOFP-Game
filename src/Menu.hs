module Menu where

import Common
import Coordinates
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (trace)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Input
import Model
import Rendering
import SDL.Font (Font)

menuWrapAround :: Int -> Int -> Int
menuWrapAround itemCount x
  | x < 0 = itemCount - 1
  | x >= itemCount = 0
  | otherwise = x

updateMenuScene :: Int -> Float -> World -> (Int, Scene)
updateMenuScene itemCount delta w@(World s@(MenuScene t p selectedItem) i@(Input _ e _) _)
  -- If Esc is pressed and we can go back one menu, then go back.
  | isKeyDown i (SpecialKey KeyEsc) && isJust p = (-1, fromMaybe s p)
  | otherwise =
    if MenuEnter `elem` e
      then (newSelectedItem, newScene)
      else (-1, newScene)
  where
    menuUpCount = count MenuUp e
    menuDownCount = count MenuDown e
    newSelectedItem = menuWrapAround itemCount (selectedItem - menuUpCount + menuDownCount)
    newScene = s {selectedItem = newSelectedItem}
-- Ignore anything that isn't a menu
updateMenuScene _ _ w@(World s _ _) = (-1, s)
