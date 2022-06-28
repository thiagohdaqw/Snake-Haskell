
module Snake where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Fixed

import World


type Head  = (Point, Float, Float)            -- (coords, velx, vely)
type Body  = [Point]
type Snake = (Head, Body)

bodySize   = 50        :: Float


-- Draw
drawNode :: Point -> Picture
drawNode (x, y) = translate x y (color blue $ circleSolid radius)
    where
        radius = bodySize/2

drawSnake :: Snake -> [Picture]
drawSnake (((x, y), _, _), body) = drawNode (x, y) : map drawNode body


-- Update
updateBody :: Point -> [Point] -> Bool -> [Point]
updateBody _ [] _  = []
updateBody (x, y) [n] b
    | b == True = replicate 2 (x, y)
    | otherwise = [(x, y)]
updateBody (x, y) (h:t) b = (x, y) : updateBody h t b

updateVel :: Float -> Float -> Float -> Float -> (Float, Float)
updateVel velx vely nx ny
    | (wallCollision x y) == True = (0, 0)
    | otherwise = (velx, vely)
    where
        x = cell2coord nx
        y = cell2coord ny

cell2coord :: Float -> Int
cell2coord c = round (c / bodySize)
