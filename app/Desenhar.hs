module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425


-- Mapa antes estava ao contrario porque no gloss o y cresce para cima, (acho que arrumei, verifica depois) !!!!

-- | Função auxiliar que ajuda a converter as posições do jogo para as posições do gloss
invertePos :: Posicao -> Posicao
invertePos (x,y) = (x,-y)

-- | Função principal para desenhar o mapa ( zip com a lista [0, -1..] porque o eixo do gloss cresce pra cima)
desenha :: ImmutableTowers -> Picture
desenha ImmutableTowers {menu = MenuInicial Jogar, imagens = [menujogar,_,_,_]} = menujogar
desenha ImmutableTowers {menu = MenuInicial Sair, imagens = [_,menusair,_,_]} = menusair
desenha ImmutableTowers {menu = ModoJogo GanhouJogo, imagens = [_,_,menuganhou,_]} = menuganhou
desenha ImmutableTowers {menu = ModoJogo PerdeuJogo, imagens = [_,_,_,menuperdeu]} = menuperdeu
desenha (ImmutableTowers _ Jogo {baseJogo = base, portaisJogo = lportais, torresJogo = ltorres, inimigosJogo = linimigos, mapaJogo = mapa} (ModoJogo EmAndamento) _)
                        =  Translate (-320) (320) $ Scale (2) (2) $ Pictures $  concatMap desenhaLinha (zip [0,-1..] mapa) ++ [desenhaBase posbase] ++ (map desenhaTorres ldesenhatorres) ++ (map desenhaPortais lposportais) ++ (map desenhaInimigo lposinimigos)
            where
                lposinimigos = map (\i -> invertePos(posicaoInimigo i)) linimigos
                lposportais = map (\i -> invertePos(posicaoPortal i)) lportais
                (lpostorres, ltipoprojtorres) = (map (\t -> invertePos(posicaoTorre t)) ltorres, map (\p -> tipoProjetil p) (map (\t -> projetilTorre t) ltorres))
                ldesenhatorres = zip lpostorres ltipoprojtorres
                posbase = invertePos(posicaoBase base)

desenhaLinha :: (Int, [Terreno]) -> [Picture]
desenhaLinha (y, linha) = concatMap (desenhaChao y) (zip [0..] linha)

desenhaChao :: Int -> (Int, Terreno) -> [Picture]
desenhaChao y (x, terreno) = [desenhaTerreno terreno (posicaoCentro x y)]

-- | Calcula a posição do centro de um quadrado no mapa ( na parte do y subtrai porque eixo cresce para baixo )
posicaoCentro :: Int -> Int -> (Float, Float)
posicaoCentro x y = (fromIntegral x + 0.5, fromIntegral y - 0.5)

-- | Função para desenhar cada tipo de terreno
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

desenhaInimigo :: Posicao -> Picture
desenhaInimigo (x,y) = Color red $ translate (x * 40) (y * 40) $ rectangleSolid 20 20

desenhaPortais :: Posicao -> Picture
desenhaPortais (x,y) = Color yellow $ translate (x * 40) (y * 40) $ circleSolid 15

desenhaTorres :: (Posicao, TipoProjetil) -> Picture
desenhaTorres ((x,y), Fogo) = desenhaTorreFogo (x,y)
desenhaTorres ((x,y), Gelo) = desenhaTorreGelo (x,y)
desenhaTorres ((x,y), Resina) = desenhaTorreResina (x,y)

desenhaTorreFogo :: Posicao -> Picture
desenhaTorreFogo (x, y) = Pictures
    [ Color (greyN 0.5) $ translate (x * 40) (y * 40) desenhaBaseTorre
    , Color red $ translate (x * 40) (y * 40 + 15) desenhaTopoTorre
    ]

desenhaTorreGelo :: Posicao -> Picture
desenhaTorreGelo (x, y) = Pictures
    [ Color (greyN 0.5) $ translate (x * 40) (y * 40) desenhaBaseTorre
    , Color blue $ translate (x * 40) (y * 40 + 15) desenhaTopoTorre
    ]

desenhaTorreResina :: Posicao -> Picture
desenhaTorreResina (x, y) = Pictures
    [ Color (greyN 0.5) $ translate (x * 40) (y * 40) desenhaBaseTorre
    , Color green $ translate (x * 40) (y * 40 + 15) desenhaTopoTorre
    ]

desenhaBaseTorre :: Picture
desenhaBaseTorre = rectangleSolid 20 30

desenhaTopoTorre :: Picture
desenhaTopoTorre = polygon [(-10,0), (10,0), (0,10)]

desenhaBase :: Posicao -> Picture
desenhaBase (x,y) = Color (greyN 0.5) $ translate (x * 40) (y * 40) $ rectangleSolid 30 30

mapa01 :: [[Terreno]]
mapa01 =
    [ [t,t,a,r,r,r,r,r],
      [r,t,a,a,r,t,t,t],
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