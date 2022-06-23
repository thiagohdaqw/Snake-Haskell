module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed

import World

type Node = (Float, Float)
type Head = (Float, Float, Float, Float)
type Body = [Node]
type Snake = (Head, Body)
type Game = (Map, Snake) 

fps = 2

width = 800     :: Int
height = 600    :: Int
blockSize = 50  :: Float

startX = (blockSize - fromIntegral width + 1) / 2 
startY = (fromIntegral height - blockSize - 1 ) / 2

offset = blockSize/2

game :: Game
game = (World.mapData, ((0, 0, 0, 0), []))

main :: IO ()
main = play
    (InWindow "Snake Map" (width, height) (600, 600))
    white
    fps
    game
    drawingFunc
    inputHandler
    updateFunc


-- Draw
colorCell 0 = red
colorCell 1 = black

drawMapCell :: Float -> Float -> Cell -> Picture
drawMapCell x y cell = translate x y $ color (colorCell cell) $ rectangleSolid blockSize blockSize

drawMap :: Map -> Float -> Float -> [Picture]
drawMap [] _ _ = []
drawMap (h:t) x y = drawMapCell x y h : drawMap t (nextX x) (nextY y)
    where
        nextX xx
            | mod' (xx + blockSize) 800 == 0 = 0
            | otherwise = xx + blockSize
        nextY yy
            | mod' (x + blockSize) 800 == 0 = yy - blockSize
            | otherwise = yy

drawNode :: Node -> Picture
drawNode (x, y) = translate x y (color blue $ rectangleSolid blockSize blockSize)

drawSnake :: Snake -> [Picture]
drawSnake ((x, y, _, _), body) = drawNode (x, y) : map drawNode body

drawingFunc :: Game -> Picture
drawingFunc (mapData, snake) = translate startX startY (pictures $ (drawMap mapData 0 0) ++ drawSnake snake)

-- Update
updateFunc :: Float -> Game -> Game
updateFunc dt (mapData, ((x, y, velx, vely), body)) = (mapData, ((newX, newY, velx, vely), newBody))
    where
        newX = x + velx*blockSize
        newY = y + vely*blockSize
        newBody = updateBody (x, y) body

        updateBody :: Node -> [Node] -> [Node]
        updateBody _ []   = []
        updateBody (x, y) (h:t) = (x, y) : updateBody h t


-- Inputs
inputHandler :: Event -> Game -> Game
inputHandler (EventKey (SpecialKey KeyUp) Down _ _)     (mapData, ((x, y, _, _), body)) = (mapData, ((x, y, 0,  1), body))
inputHandler (EventKey (SpecialKey KeyDown) Down _ _)   (mapData, ((x, y, _, _), body)) = (mapData, ((x, y, 0,  -1), body))
inputHandler (EventKey (SpecialKey KeyRight) Down _ _)  (mapData, ((x, y, _, _), body)) = (mapData, ((x, y, 1,  0), body))
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _)   (mapData, ((x, y, _, _), body)) = (mapData, ((x, y, -1,  0), body))
inputHandler (EventKey (Char 'x') Down _ _)             (mapData, ((x, y, xx, yy), [])) = (mapData, ((x, y, xx,  yy), [(x, y)]))
inputHandler (EventKey (Char 'x') Down _ _)             (mapData, ((x, y, xx, yy), body)) = (mapData, ((x, y, xx, yy), body ++ [last body]))
inputHandler _ w = w
