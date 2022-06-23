module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

windowDisplay :: Display
windowDisplay = InWindow "Window" (600, 600) (600, 600)

type Node = (Float, Float)
type Head = (Float, Float, Float, Float, [Node])

block = 50
fps = 5

main :: IO ()
main = play
    windowDisplay
    white
    fps
    (0, 0, 0, 0, [])
    drawingFunc
    inputHandler
    updateFunc

drawNode :: Node -> Picture
drawNode (x, y) = translate x y (color red $ rectangleWire block block)

drawingFunc :: Head -> Picture
drawingFunc (x, y, _, _, body) = pictures $ drawNode (x, y) : map drawNode body

inputHandler :: Event -> Head -> Head
inputHandler (EventKey (SpecialKey KeyUp) Down _ _)     (x, y, _, _, body) = (x, y, 0,  1, body)
inputHandler (EventKey (SpecialKey KeyDown) Down _ _)   (x, y, _, _, body) = (x, y, 0, -1, body)
inputHandler (EventKey (SpecialKey KeyRight) Down _ _)  (x, y, _, _, body) = (x, y, 1,  0, body)
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _)   (x, y, _, _, body) = (x, y, -1, 0, body)
inputHandler (EventKey (Char 'x') Down _ _)             (x, y, xx, yy, []) = (x, y, xx, yy, [(x, y)])
inputHandler (EventKey (Char 'x') Down _ _)             (x, y, xx, yy, body) = (x, y, xx, yy, body ++ [last body])
inputHandler _ w = w

updateFunc :: Float -> Head -> Head
updateFunc dt (x, y, velx, vely, body) = (newX, newY, velx, vely, newBody)
    where
        newX = x + velx*block
        newY = y + vely*block
        newBody = updateBody (x, y) body

        updateBody :: Node -> [Node] -> [Node]
        updateBody _ []   = []
        updateBody (x, y) (h:t) = (x, y) : updateBody h t
