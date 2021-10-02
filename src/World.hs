module World where

import Assets
import Graphics.Gloss
import Model
import Rendering
import SDL.Font (Font)
import Space

initWorld :: Assets -> Font -> World
initWorld a = World a (Intro 2.5)

initMainMenu :: Scene
initMainMenu = MainMenu

updateWorld :: Float -> World -> IO World
updateWorld d (World a s b) = return $ World a (updateScene d s) b

updateScene :: Float -> Scene -> Scene
updateScene d (Intro dt)
  | newDisplayTimer > 0 = Intro newDisplayTimer -- Keep displaying the Intro screen
  | otherwise = initMainMenu -- Go to main menu
  where
    newDisplayTimer = dt - d
updateScene d s = s

renderWorldScaled :: World -> IO Picture
renderWorldScaled w = do
  world <- renderWorld w
  let rs = fromIntegral windowScale
   in return $ scale rs rs world

renderWorld :: World -> IO Picture
renderWorld (World a (Intro _) _) = return $ getAsset "intro" a
renderWorld (World a Test _) =
  return $
    pictures
      [ translate (-110) (-70) (renderDbgString 0.25 green "-110,-70"),
        translate 0 0 (renderDbgString 0.25 green "0,0"),
        translate (-110) 70 (renderDbgString 0.25 green "-110,70"),
        translate 90 (-70) (renderDbgString 0.25 green "90,-70"),
        translate 90 70 (renderDbgString 0.25 green "90,70")
      ]
renderWorld (World a MainMenu f) = do
  gameTxt <- renderString f red "Game"
  subTxt <- renderString f white "UU-INFOFP"
  startTxt <- renderString f violet "Start"
  testTxt <- renderString f violet "Test"
  quitTxt <- renderString f violet "Quit"
  return $
    pictures
      [ setPos 120 60 $ scale 4 4 gameTxt,
        setPos 128 48 subTxt,
        setPos 120 94 startTxt,
        setPos 120 106 testTxt,
        setPos 120 118 quitTxt
      ]
renderWorld _ = return $ renderDbgString 1 red "Not implemented"
