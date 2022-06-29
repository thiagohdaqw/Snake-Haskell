module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed

import World
import Coin
import Snake
import Menu


data State =
      START
    | GAME
    | END
    deriving (Enum, Show, Eq, Ord)

type Game  = (Map, Snake, State) 


fps = 5

width       = 800       :: Float
height      = 600       :: Float
cellSize    = 50        :: Float

startX = (cellSize - width)  / 2.0
startY = (height - cellSize) / 2.0

title  = "Snake Game"

coinPosition = (10*cellSize, -10*cellSize)

game :: Game
game = (World.mapData, (((1*cellSize, -1*cellSize), 0, 0), [(0, 0)]), START)

window :: Display
window = (InWindow title (iwidth, iheight) (0, 0))
    where
        iwidth  = round width
        iheight = round height

main :: IO ()
main = play
    window
    black
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


drawingFunc :: Game -> Picture
drawingFunc (_, _, START) = Menu.drawMenu width
drawingFunc (mapData, snake, GAME) = translate startX startY (pictures $ (drawMap mapData 0 0) ++ [drawCoin coinPosition] ++ drawSnake snake)

-- Update
updateFunc :: Float -> Game -> Game
updateFunc dt (mapData, (((x, y), velx, vely), body), GAME) = (mapData, (((newX, newY), newVelx, newVely), newBody), GAME)
    where
        nextX = x + velx*cellSize
        nextY = y + vely*cellSize

        (newVelx, newVely) = updateVel velx vely nextX nextY
        newX = x + newVelx*cellSize
        newY = y + newVely*cellSize
        newBody = updateBody (x, y) body (coinCollision (x, y))
updateFunc dt game = game

coinCollision :: Point -> Bool
coinCollision n = n == coinPosition

-- Inputs
inputHandler :: Event -> Game -> Game
inputHandler event (mapa, snake, START) = startInputHandler event (mapa, snake, START)
inputHandler event (mapa, snake, GAME) = gameInputHandler event (mapa, snake, GAME)
inputHandler _ g = g

startInputHandler :: Event -> Game -> Game
startInputHandler (EventKey (Char 's') Down _ _) (mapa, snake, _) = (mapa, snake, GAME)
startInputHandler _ g = g

gameInputHandler :: Event -> Game -> Game
gameInputHandler (EventKey (SpecialKey KeyUp) Down _ _)     (mapData, (((x, y), _, _), body), state) = (mapData, (((x, y), 0,  1),  body), state)
gameInputHandler (EventKey (SpecialKey KeyDown) Down _ _)   (mapData, (((x, y), _, _), body), state) = (mapData, (((x, y), 0,  -1), body), state)
gameInputHandler (EventKey (SpecialKey KeyRight) Down _ _)  (mapData, (((x, y), _, _), body), state) = (mapData, (((x, y), 1,  0),  body), state)
gameInputHandler (EventKey (SpecialKey KeyLeft) Down _ _)   (mapData, (((x, y), _, _), body), state) = (mapData, (((x, y), -1,  0), body), state)
gameInputHandler (EventKey (Char 'x') Down _ _)             (mapData, (((x, y), xx, yy), []), state) = (mapData, (((x, y), xx,  yy), [(x, y)]), state)
gameInputHandler (EventKey (Char 'x') Down _ _)             (mapData, (((x, y), xx, yy), body), state) = (mapData, (((x, y), xx, yy), body ++ [last body]), state)
gameInputHandler _ g = g
