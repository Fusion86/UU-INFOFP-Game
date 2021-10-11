module Menu where

import Common
import Graphics.Gloss.Interface.IO.Game
import Input
import Model
import SDL.Font (Font)

menuWrapAround :: Int -> Int -> Int
menuWrapAround itemCount x
  | x < 0 = itemCount - 1
  | x >= itemCount = 0
  | otherwise = x

updateMenuScene :: Int -> Float -> World -> (Maybe Int, Scene)
updateMenuScene itemCount delta w@(World s@(MenuScene t p selectedItem) i@(Input _ ev _))
  -- If Esc is pressed and we can go back one menu, then go back.
  | MenuBack `elem` ev, Just p <- p = (Nothing, p)
  -- If enter is pressed then return the id of the selected menu.
  | MenuEnter `elem` ev = (Just newSelectedItem, newScene)
  -- Else just return the new scene with the updated selectedItem.
  | otherwise = (Nothing, newScene)
  where
    menuUpCount = count MenuUp ev
    menuDownCount = count MenuDown ev
    newSelectedItem = menuWrapAround itemCount (selectedItem - menuUpCount + menuDownCount)
    newScene = s {selectedItem = newSelectedItem}
-- Ignore anything that isn't a menu
updateMenuScene _ _ w@(World s _) = (Nothing, s)
