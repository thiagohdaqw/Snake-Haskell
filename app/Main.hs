module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed

import World
import Coin

type Node = (Float, Float)                   -- (x, y)
type Head  = (Node, Float, Float)            -- (coords, velx, vely)
type Body  = [Node]
type Snake = (Head, Body)
type Game  = (Map, Snake) 

fps = 5

width       = 800       :: Float
height      = 600       :: Float
cellSize    = 50        :: Float

startX = (cellSize - width)  / 2.0
startY = (height - cellSize) / 2.0

title  = "Snake Game"

coinPosition = (10*cellSize, -10*cellSize)

game :: Game
game = (World.mapData, (((1*cellSize, -1*cellSize), 0, 0), [(0, 0)]))

window :: Display
window = (InWindow title (iwidth, iheight) (0, 0))
    where
        iwidth  = round width
        iheight = round height

main :: IO ()
main = play
    window
    white
    fps
    game
    drawingFunc
    inputHandler
    updateFunc


-- Draw
drawMapCell :: Float -> Float -> Cell -> Picture
drawMapCell x y cell = translate x y $ color (colorCell cell) $ rectangleSolid cellSize cellSize

drawMap :: Map -> Float -> Float -> [Picture]
drawMap [] _ _ = []
drawMap (h:t) x y = drawMapCell x y h : drawMap t (nextX x) (nextY y)
    where
        nextX xx
            | mod' (xx + cellSize) width == 0 = 0
            | otherwise = xx + cellSize
        nextY yy
            | mod' (x + cellSize) width == 0 = yy - cellSize
            | otherwise = yy

drawNode :: Node -> Picture
drawNode (x, y) = translate x y (color blue $ circleSolid radius)
    where
        radius = cellSize/2

drawSnake :: Snake -> [Picture]
drawSnake (((x, y), _, _), body) = drawNode (x, y) : map drawNode body

drawingFunc :: Game -> Picture
drawingFunc (mapData, snake) = translate startX startY (pictures $ (drawMap mapData 0 0) ++ [drawCoin coinPosition] ++ drawSnake snake)

-- Update
updateFunc :: Float -> Game -> Game
updateFunc dt (mapData, (((x, y), velx, vely), body)) = (mapData, (((newX, newY), newVelx, newVely), newBody))
    where
        nextX = x + velx*cellSize
        nextY = y + vely*cellSize

        (newVelx, newVely) = updateVel velx vely nextX nextY
        newX = x + newVelx*cellSize
        newY = y + newVely*cellSize
        newBody = updateBody (x, y) body (coinCollision (x, y))

coinCollision :: Node -> Bool
coinCollision n = n == coinPosition

updateBody :: Node -> [Node] -> Bool -> [Node]
updateBody _ [] _  = []
updateBody (x, y) [n] b
    | b == True = replicate 2 (x, y)
    | otherwise = [(x, y)]
updateBody (x, y) (h:t) b = (x, y) : updateBody h t b

updateVel :: Float -> Float -> Float -> Float -> (Float, Float)
updateVel velx vely nx ny
    | (wallCollision x y) == True = (0, 0)
    | otherwise                             = (velx, vely)
    where
        x = cell2coord nx
        y = cell2coord ny

cell2coord :: Float -> Int
cell2coord c = round (c / cellSize)

-- Inputs
inputHandler :: Event -> Game -> Game
inputHandler (EventKey (SpecialKey KeyUp) Down _ _)     (mapData, (((x, y), _, _), body)) = (mapData, (((x, y), 0,  1),  body))
inputHandler (EventKey (SpecialKey KeyDown) Down _ _)   (mapData, (((x, y), _, _), body)) = (mapData, (((x, y), 0,  -1), body))
inputHandler (EventKey (SpecialKey KeyRight) Down _ _)  (mapData, (((x, y), _, _), body)) = (mapData, (((x, y), 1,  0),  body))
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _)   (mapData, (((x, y), _, _), body)) = (mapData, (((x, y), -1,  0), body))
inputHandler (EventKey (Char 'x') Down _ _)             (mapData, (((x, y), xx, yy), [])) = (mapData, (((x, y), xx,  yy), [(x, y)]))
inputHandler (EventKey (Char 'x') Down _ _)             (mapData, (((x, y), xx, yy), body)) = (mapData, (((x, y), xx, yy), body ++ [last body]))
inputHandler _ g = g
