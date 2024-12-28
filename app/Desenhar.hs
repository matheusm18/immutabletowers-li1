module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425

-- Função principal para desenhar o mapa
desenha :: ImmutableTowers -> Picture
desenha (ImmutableTowers Jogo {baseJogo = base, portaisJogo = lportais, torresJogo = ltorres, inimigosJogo = linimigos, mapaJogo = mapa}) 
                        = Pictures $  concatMap desenhaLinha (zip [0..] mapa) ++ [desenhaBase posbase] ++ (map desenhaTorres lpostorres) ++ (map desenhaPortais lposportais) ++ (map desenhaInimigo lposinimigos)
            where
                lposinimigos = map (\i -> posicaoInimigo i) linimigos
                lposportais = map (\i -> posicaoPortal i) lportais
                lpostorres = map (\i -> posicaoTorre i) ltorres
                posbase = posicaoBase base

desenhaLinha :: (Int, [Terreno]) -> [Picture]
desenhaLinha (y, linha) = concatMap (desenhaChao y) (zip [0..] linha)

desenhaChao :: Int -> (Int, Terreno) -> [Picture]
desenhaChao y (x, terreno) = [desenhaTerreno terreno (posicaoCentro x y)]

-- Calcula a posição do centro de um quadrado no mapa
posicaoCentro :: Int -> Int -> (Float, Float)
posicaoCentro x y = (fromIntegral x + 0.5, fromIntegral y + 0.5)

-- Função para desenhar cada tipo de terreno
desenhaTerreno :: Terreno -> Posicao -> Picture
desenhaTerreno Agua (x, y) = desenhaChaoBase (x, y) (makeColorI 154 209 221 255)
desenhaTerreno Relva (x, y) = desenhaChaoBase (x, y) (makeColorI 91 142 80 255)
desenhaTerreno Terra (x, y) = desenhaChaoBase (x, y) (makeColorI 186 140 93 255)

desenhaChaoBase :: Posicao -> Color -> Picture
desenhaChaoBase (x, y) color = Color color $ translate (x * w) (y * h) $ rectangleSolid w h

w :: Float
w = 40

h :: Float
h = 40

-- tentar perceber porque o mapa esta ao contrario!!

desenhaInimigo :: Posicao -> Picture
desenhaInimigo (x,y) = Color red $ translate (x * 40) (y * 40) $ rectangleSolid 20 20

desenhaPortais :: Posicao -> Picture
desenhaPortais (x,y) = Color yellow $ translate (x * 40) (y * 40) $ circleSolid 15

desenhaTorres :: Posicao -> Picture
desenhaTorres (x,y) = Color (orange) $ translate (x * 40) (y * 40) $ rectangleSolid 20 30

desenhaBase :: Posicao -> Picture
desenhaBase (x,y) = Color (greyN 0.5) $ translate (x * 40) (y * 40) $ rectangleSolid 30 30

mapa01 :: [[Terreno]]
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