module Desenhar where

import Graphics.Gloss
import ImmutableTowers



desenha :: ImmutableTowers -> Picture
desenha (ImmutableTowers m b) = Pictures $ desenhaterreno (mapaCoord (0.5,0.5) m) m ++ desenhaBase posBase

  where

    desenhaterreno :: [(Terreno, Posicao)] -> Mapa -> [Picture]
    desenhaterreno [] _ = []
    desenhaterreno ((t,(x,y)):r) mapa
     |t == Terra && aguacimabaixo (x,y) mapa = desenhapontehorizontal (x,y) ++ desenhaterreno r mapa
     |t == Terra && aguaesqdir (x,y) mapa = desenhapontevertical (x,y) ++ desenhaterreno r mapa
     |t == Terra = desenhaterra (x,y) ++ desenhaterreno r mapa
     |t == Relva = desenharelva (x,y) ++ desenhaterreno r mapa
     |otherwise = desenhaagua (x,y) ++ desenhaterreno r mapa

    
    desenhaterra :: Posicao  -> [Picture]
    desenhaterra (x,y) = [desenhaterradireita (x,y)] ++ [desenhaterraesquerda (x,y)] ++ [desenhaterramain (x,y)]
    desenhaterramain :: Posicao -> Picture 
    desenhaterramain (x,y) = Color (makeColorI 186 140 93 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h) $ polygon [(-w/2,0), (0,h/2), (w/2,0), (0,-h/2)]
    desenhaterradireita :: Posicao -> Picture
    desenhaterradireita (x,y) = Color (makeColorI 129 96 62 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(w/2,0), (w/2,-h/4), (0,-h/2-h/4), (0,-h/2)]
    desenhaterraesquerda :: Posicao -> Picture
    desenhaterraesquerda (x,y) = Color (makeColorI 167 125 83 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(-w/2,0), (-w/2,-h/4), (0,-h/2-h/4), (0,-h/2)]

    desenhaagua :: Posicao  -> [Picture]
    desenhaagua (x,y) = [desenhaaguadireita (x,y)] ++ [desenhaaguaesquerda (x,y)] ++ [desenhaaguamain (x,y)]
    desenhaaguamain :: Posicao -> Picture 
    desenhaaguamain (x,y) = Color (makeColorI 154 209 221 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(-w/2,0), (0,h/2), (w/2,0), (0,-h/2)]
    desenhaaguadireita :: Posicao -> Picture
    desenhaaguadireita (x,y) = Color (makeColorI 121 194 210 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(w/2,0), (w/2,-h/4), (0,-h/2-h/4), (0,-h/2)]
    desenhaaguaesquerda :: Posicao -> Picture
    desenhaaguaesquerda (x,y) = Color (makeColorI 143 204 217 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(-w/2,0), (-w/2,-h/4), (0,-h/2-h/4), (0,-h/2)]

    desenharelva :: Posicao  -> [Picture]
    desenharelva (x,y) = [desenharelvadireita (x,y)] ++ [desenharelvaesquerda (x,y)] ++ [desenharelvamain (x,y)]
    desenharelvamain :: Posicao -> Picture 
    desenharelvamain (x,y) = Color (makeColorI 91 142 80 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(-w/2,0), (0,h/2), (w/2,0), (0,-h/2)]
    desenharelvadireita :: Posicao -> Picture
    desenharelvadireita (x,y) = Color (makeColorI 61 97 53 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(w/2,0), (w/2,-h/4), (0,-h/2-h/4), (0,-h/2)]
    desenharelvaesquerda :: Posicao -> Picture
    desenharelvaesquerda (x,y) = Color (makeColorI 81 127 71 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(-w/2,0), (-w/2,-h/4), (0,-h/2-h/4), (0,-h/2)]


    desenhapontehorizontal :: Posicao  -> [Picture]
    desenhapontehorizontal (x,y) = [desenhapontemain (x,y)] ++ [desenharailpontedirh (x,y)] ++ [desenharailponteesqh (x,y)] ++ [desenhaponteaguadireita (x,y)] ++ [desenhaponteaguaesquerda (x,y)] ++ [desenhapontedireita (x,y)] ++ [desenhaponteesquerda (x,y)] 
    desenhapontemain :: Posicao -> Picture 
    desenhapontemain (x,y) = Color (makeColorI 161 157 158 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(-w/2,0), (0,h/2), (w/2,0), (0,-h/2)]
    desenharailponteesqh :: Posicao -> Picture
    desenharailponteesqh (x,y) = Color (makeColorI 77 46 28 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(0,3*h/4), (0,h/2), (w/2,0), (w/2,h/4)]
    desenharailpontedirh :: Posicao -> Picture
    desenharailpontedirh (x,y) = Color (makeColorI 77 46 28 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(-w/2,h/4), (-w/2,0), (0,-h/2), (0,-h/4)]
    desenhaponteesquerda:: Posicao -> Picture
    desenhaponteesquerda (x,y) = Color (makeColorI 103 104 109 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(-w/2,0), (-w/2,-h/8), (0,-h/2-h/8), (0,-h/2)]
    desenhaponteaguaesquerda :: Posicao -> Picture
    desenhaponteaguaesquerda (x,y) = Color (makeColorI 143 204 217 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(-w/2,0), (-w/2,-h/4), (0,-h/2-h/4), (0,-h/2)]
    desenhapontedireita :: Posicao -> Picture
    desenhapontedireita (x,y) = Color (makeColorI 103 104 109 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(w/2,0), (w/2,-h/8), (0,-h/2-h/8), (0,-h/2)]
    desenhaponteaguadireita :: Posicao -> Picture
    desenhaponteaguadireita (x,y) = Color (makeColorI 121 194 210 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(w/2,0), (w/2,-h/4), (0,-h/2-h/4), (0,-h/2)]


    desenhapontevertical :: Posicao  -> [Picture]
    desenhapontevertical (x,y) = [desenhapontemain (x,y)] ++ [desenharailpontedirv (x,y)] ++ [desenharailponteesqv (x,y)] ++ [desenhaponteaguadireita (x,y)] ++ [desenhaponteaguaesquerda (x,y)] ++ [desenhapontedireita (x,y)] ++ [desenhaponteesquerda (x,y)] 
    desenharailponteesqv :: Posicao -> Picture
    desenharailponteesqv (x,y) = Color (makeColorI 77 46 28 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [(-w/2,h/4), (-w/2,0),(0,h/2), (0,3*h/4)]
    desenharailpontedirv :: Posicao -> Picture
    desenharailpontedirv (x,y) = Color (makeColorI 77 46 28 255) $ translate (x*w/2 - y*w/2) (-x*h/2 - y*h/2 + 4*h)  $ polygon [ (w/2,0), (w/2,h/4), (0,-h/4), (0,-h/2)]

    desenhaBase :: Posicao -> [Picture]
    desenhaBase pos = desenhaBase1 pos ++ desenhaBase2 pos ++ desenhaBase3 pos ++ desenhaBase4 pos ++ desenhaTelhado1 pos ++ desenhaTelhado2 pos ++ desenhaBandeiraPole pos ++ desenhaBandeira pos


    desenhaBase1 :: Posicao -> [Picture]
    desenhaBase1 (x, y) = [
        translate (x * w / 2 - y * w / 2) (-x * h / 2 - y * h / 2 + 4 * h) $
        Color (makeColorI 174 184 174 255) $
        Pictures [polygon [(-w/2, 0), (-w/2, h/8), (0, -h/2 + h/8), (0, -h/2)], 
                  polygon [(-w/2 + w/8, h/2), (-w/2 + w/8, h/2 + h/8), (0, h/4), (0, h/8)]]
      ]

    desenhaBase2 :: Posicao -> [Picture]
    desenhaBase2 (x, y) = [
        translate (x * w / 2 - y * w / 2) (-x * h / 2 - y * h / 2 + 4 * h) $
        Color (makeColorI 207 218 206 255) $
        polygon [(-w/2, h/8), (-w/2 + w/8, h/2), (0, h/8), (0, -h/2 + h/ 8)]
        ]

    desenhaBase3 :: Posicao -> [Picture]
    desenhaBase3 (x, y) = [
        translate (x * w / 2 - y * w / 2) (-x * h / 2 - y * h / 2 + 4 * h) $
        Color (makeColorI 135 142 134 255) $
        Pictures [polygon [(w/2, 0), (w/2, h/8), (0, -h/2 + h/8), (0, -h/2)], 
                  polygon [(w/2 - w/8, h/2), (w/2 - w/8, h/2 + h/8), (0, h/4), (0, h/8)]]
      ]

    desenhaBase4 :: Posicao -> [Picture]
    desenhaBase4 (x, y) = [
        translate (x * w / 2 - y * w / 2) (-x * h / 2 - y * h / 2 + 4 * h) $
        Color (makeColorI 182 191 181 255) $
        polygon [(w/2, h/8), (w/2 - w/8, h/2), (0, h/8), (0, -h/2 + h/ 8)]
      ]

    desenhaTelhado1 :: Posicao -> [Picture]
    desenhaTelhado1 (x, y) = [
        translate (x * w / 2 - y * w / 2) (-x * h / 2 - y * h / 2 + 4 * h) $
        Color (makeColorI 213 101 101 255) $
        polygon [(-w/2 + w/8, h/2 + h/8), (0, h), (0, h/ 4)]
      ]

    desenhaTelhado2 :: Posicao -> [Picture]
    desenhaTelhado2 (x, y) = [
        translate (x * w / 2 - y * w / 2) (-x * h / 2 - y * h / 2 + 4 * h) $
        Color (makeColorI 188 88 88 255) $
        polygon [(w/2 - w/8, h/2 + h/8), (0, h), (0, h/ 4)]
      ]

    desenhaBandeiraPole :: Posicao -> [Picture]
    desenhaBandeiraPole (x, y) = [
        translate (x * w / 2 - y * w / 2) (-x * h / 2 - y * h / 2 + 4 * h) $
        Color (makeColorI 139 69 19 255) $
        polygon [(-h/64, h-h/32), (-h/64, 5*h/4), (h/64, 5*h/4), (h/64, h-h/32)]
      ]

    desenhaBandeira :: Posicao -> [Picture]
    desenhaBandeira (x, y) = [
        translate (x * w / 2 - y * w / 2) (-x * h / 2 - y * h / 2 + 4 * h) $
        Color red $
        polygon [(-h/64, 9*h/8), (-h/64, 5*h/4), (-h/4, 5*h/4), (-h/8, 19*h/16), (-h/4, 9*h/8)]
      ]

    posBase = posicaoBase b

w = 100
h = w/2


mapa01 :: Mapa
mapa01 =
    [ [t,t,a,r,r,r,r,r],
      [a,t,a,a,r,t,t,t],
      [a,t,a,a,r,t,r,a],
      [a,t,a,a,r,t,r,a],
      [t,t,a,a,r,t,t,t],
      [t,a,a,a,r,r,r,t],
      [t,a,t,t,t,t,t,t],
      [t,t,t,a,r,r,r,a]
                      ] 
    where
        t = Terra
        r = Relva
        a = Agua


contaCol :: Mapa -> Int
contaCol ([] : b) = 0
contaCol ((a : b):ab) = 1 + contaCol (b:ab)

contaLinh :: Mapa -> Int
contaLinh [] = 0
contaLinh ((a : b):ab) = 1 + contaCol ab

mapaCoord :: Posicao -> Mapa -> [(Terreno,Posicao)]
mapaCoord _ [] = []
mapaCoord (x, y) ([] : b) = mapaCoord (0.5, y + 1) (b)
mapaCoord (x, y) ((a : b):ab) = (a,(x, y)) : mapaCoord (x + 1, y) ((b) : ab)


aguacimabaixo :: Posicao -> Mapa -> Bool
aguacimabaixo (x,y) mapa = length ((filter (==(Agua, (x,y-1)))(mapaCoord (0.5, 0.5) mapa)) ++ (filter (==(Agua, (x,y+1)))(mapaCoord (0.5, 0.5) mapa))) == 2 

aguaesqdir :: Posicao -> Mapa -> Bool
aguaesqdir(x,y) mapa = length ((filter (==(Agua, (x+1,y)))(mapaCoord (0.5, 0.5) mapa)) ++ (filter (==(Agua, (x-1,y)))(mapaCoord (0.5, 0.5) mapa))) == 2 


main :: IO ()
main = display
            (InWindow "Immutable Towers" (((round w)+10)*(contaCol mapa01),((round h)+10) * (contaLinh mapa01)) (500, 250))
            black
            (desenha (ImmutableTowers mapa01 (Base 100 (7.5,1.5) 0)))
