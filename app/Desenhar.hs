module Desenhar where

import Graphics.Gloss
import ImmutableTowers


type Posicao = (Float, Float)

desenha :: ImmutableTowers -> Picture
desenha (ImmutableTowers m) = Pictures $ (desenhaterreno (mapaCoord (0,0) m))
  where

    desenhaterreno :: [(Terreno, Posicao)] -> [Picture]
    desenhaterreno [] = []
    desenhaterreno ((t,(x,y)):r)
     |t == Terra = desenhaterra (x,y) ++ desenhaterreno r
     |t == Relva = desenharelva (x,y) ++ desenhaterreno r
     |otherwise = desenhaagua (x,y) ++ desenhaterreno r

    
    desenhaterra :: Posicao  -> [Picture]
    desenhaterra (x,y) = [desenhaterradireita (x,y)] ++ [desenhaterraesquerda (x,y)] ++ [desenhaterramain (x,y)]
    desenhaterramain :: Posicao -> Picture 
    desenhaterramain (x,y) = Color (makeColorI 186 140 93 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2) $ polygon [(-w,0), (0,h), (w,0), (0,-h)]
    desenhaterradireita :: Posicao -> Picture
    desenhaterradireita (x,y) = Color (makeColorI 129 96 62 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2)  $ polygon [(w,0), (w,-h/4), (0,-h-h/4), (0,-h)]
    desenhaterraesquerda :: Posicao -> Picture
    desenhaterraesquerda (x,y) = Color (makeColorI 167 125 83 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2)  $ polygon [(-w,0), (-w,-h/4), (0,-h-h/4), (0,-h)]

    desenhaagua :: Posicao  -> [Picture]
    desenhaagua (x,y) = [desenhaaguadireita (x,y)] ++ [desenhaaguaesquerda (x,y)] ++ [desenhaaguamain (x,y)]
    desenhaaguamain :: Posicao -> Picture 
    desenhaaguamain (x,y) = Color (makeColorI 154 209 221 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2)  $ polygon [(-w,0), (0,h), (w,0), (0,-h)]
    desenhaaguadireita :: Posicao -> Picture
    desenhaaguadireita (x,y) = Color (makeColorI 121 194 210 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2)  $ polygon [(w,0), (w,-h/4), (0,-h-h/4), (0,-h)]
    desenhaaguaesquerda :: Posicao -> Picture
    desenhaaguaesquerda (x,y) = Color (makeColorI 143 204 217 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2)  $ polygon [(-w,0), (-w,-h/4), (0,-h-h/4), (0,-h)]

    desenharelva :: Posicao  -> [Picture]
    desenharelva (x,y) = [desenharelvadireita (x,y)] ++ [desenharelvaesquerda (x,y)] ++ [desenharelvamain (x,y)]
    desenharelvamain :: Posicao -> Picture 
    desenharelvamain (x,y) = Color (makeColorI 91 142 80 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2)  $ polygon [(-w,0), (0,h), (w,0), (0,-h)]
    desenharelvadireita :: Posicao -> Picture
    desenharelvadireita (x,y) = Color (makeColorI 61 97 53 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2)  $ polygon [(w,0), (w,-h/4), (0,-h-h/4), (0,-h)]
    desenharelvaesquerda :: Posicao -> Picture
    desenharelvaesquerda (x,y) = Color (makeColorI 81 127 71 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2)  $ polygon [(-w,0), (-w,-h/4), (0,-h-h/4), (0,-h)]


    w = 100
    h = w/2



mapa01 :: Mapa
mapa01 =
    [ [a, t, r, a, a, a],
      [r, t, r, a, r, r],
      [r, t, r, a, r, t],
      [r, t, r, a, r, t],
      [r, t, t, t, t, t],
      [a, a, a, a, r, r]
                       ] 
    where
        t = Terra
        r = Relva
        a = Agua




mapaCoord :: Posicao -> Mapa -> [(Terreno,Posicao)]
mapaCoord _ [] = []
mapaCoord (x, y) ([] : b) = mapaCoord (0, y + 1) (b)
mapaCoord (x, y) ((a : b):ab) = (a,(x, y)) : mapaCoord (x + 1, y) ((b) : ab)


main :: IO ()
main = display FullScreen black (desenha (ImmutableTowers mapa01))
