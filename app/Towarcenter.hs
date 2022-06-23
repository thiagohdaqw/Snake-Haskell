module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (600, 600)

type World = (Float, Float)

main :: IO ()
main = play
    windowDisplay
    white
    20
    (0, 0)
    drawingFunc
    inputHandler
    updateFunc

drawingFunc :: World -> Picture
drawingFunc (x, y) = translate x y (Circle 20)

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) (x, y)= (x, y + 10)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) (x, y)= (x, y - 10)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) (x, y)= (x + 10, y)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) (x, y)= (x - 10, y)
inputHandler _ w = w

updateFunc :: Float -> World -> World
updateFunc dt (x, y) = (towardCenter x, towardCenter y)
    where
        towardCenter :: Float -> Float
        towardCenter c
            | abs c < 0.25 = 0
            | c > 0      = c - 0.25
            | otherwise  = c + 0.25
