module Menu where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

drawMenu :: Float -> Picture
drawMenu width = color white $ pictures [drawTitle width, drawSubTitle width]


drawTitle :: Float -> Picture
drawTitle width = translate (-width/2) 0 $ text "Snake-Game"

drawSubTitle :: Float -> Picture
drawSubTitle width = scale 0.2 0.2 $ translate (-width) (-150) $ text "Aperte S para iniciar"
