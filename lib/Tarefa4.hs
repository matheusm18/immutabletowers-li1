module Tarefa4 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12425
import Tarefa1
import Tarefa2
import Tarefa3

window :: Display
window = InWindow "Tarefa 4" (640, 420) (300,300)

background :: Color
background = blue

frameRate 10



main = window background frameRate jogoInicio desenha reageEventos reageTempo
