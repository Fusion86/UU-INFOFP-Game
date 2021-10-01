module World where

import Assets
import Graphics.Gloss
import Model
import Rendering

initWorld :: Assets -> World
initWorld a = World a (Intro 2.5)

initMainMenu :: Scene
initMainMenu = MainMenu

updateWorld :: Float -> World -> World
updateWorld d (World a s) = World a (updateScene d s)

updateScene :: Float -> Scene -> Scene
updateScene d (Intro dt)
  | newDisplayTimer > 0 = Intro newDisplayTimer -- Keep displaying the Intro screen
  | otherwise = initMainMenu -- Go to main menu
  where
    newDisplayTimer = dt - d
updateScene d s = s

renderWorldScaled :: World -> Picture
renderWorldScaled =
  let rs = fromIntegral windowScale
   in scale rs rs . renderWorld

renderWorld :: World -> Picture
renderWorld (World a (Intro _)) = getAsset "intro" a
renderWorld (World a Test) =
  pictures
    [ translate (-110) (-70) (renderString 0.25 green "-110,-70"),
      translate 0 0 (renderString 0.25 green "0,0"),
      translate (-110) 70 (renderString 0.25 green "-110,70"),
      translate 90 (-70) (renderString 0.25 green "90,-70"),
      translate 90 70 (renderString 0.25 green "90,70")
    ]
renderWorld (World a MainMenu) = renderString 1 green "Main Menu"
renderWorld _ = renderString 1 red "Not implemented"
