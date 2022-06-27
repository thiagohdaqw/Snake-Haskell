module Coin where

import Graphics.Gloss


type Coin = (Float, Float)

drawCoin :: Coin -> Picture
drawCoin (x, y) = translate x y $ color white $ circleSolid 25

