module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425


-- Mapa antes estava ao contrario porque no gloss o y cresce para cima, (acho que arrumei, verifica depois) !!!!

-- | Função auxiliar que ajuda a converter as posições do jogo para as posições do gloss
invertePos :: Posicao -> Posicao
invertePos (x,y) = (x,-y)


-- | Função auxiliar que busca a imagem que esta associada a string na lista de imagens
getImagem :: String -> [(String,Picture)] -> Picture
getImagem s lista = case lookup s lista of
    Just p -> p
    Nothing -> Blank

-- | Função principal para desenhar o mapa ( zip com a lista [0, -1..] porque o eixo do gloss cresce pra cima)
desenha :: ImmutableTowers -> Picture
desenha ImmutableTowers {menu = MenuInicial Jogar, imagens = limagens} = getImagem "menujogar" limagens
desenha ImmutableTowers {menu = MenuInicial Sair, imagens = limagens} = getImagem "menusair" limagens
desenha ImmutableTowers {menu = ModoJogo GanhouJogo, imagens = limagens} = getImagem "menuganhou" limagens
desenha ImmutableTowers {menu = ModoJogo PerdeuJogo, imagens = limagens} = getImagem "menuperdeu" limagens
desenha (ImmutableTowers _ Jogo {baseJogo = base, portaisJogo = lportais, torresJogo = ltorres, inimigosJogo = linimigos, mapaJogo = mapa} (ModoJogo EmAndamento) limagens)
            =  Pictures [getImagem "bgjogo" limagens,
                        (Translate (-320) (240) $ Scale (2) (2) $ Pictures $  concatMap desenhaLinha (zip [0,-1..] mapa) ++ [desenhaBase posbase] ++ (map (desenhaTorres torres) ldesenhatorres) ++ (map (desenhaPortais portal) lposportais) ++ (map (desenhaInimigo inimigoeste) lposinimigos)),
                         picvida,
                         piccreditos]
            where
                lposinimigos = map (\i -> invertePos(posicaoInimigo i)) linimigos
                lposportais = map (\i -> invertePos(posicaoPortal i)) lportais
                (lpostorres, ltipoprojtorres) = (map (\t -> invertePos(posicaoTorre t)) ltorres, map (\p -> tipoProjetil p) (map (\t -> projetilTorre t) ltorres))
                ldesenhatorres = zip lpostorres ltipoprojtorres
                posbase = invertePos(posicaoBase base)
                picvida = Scale 0.5 0.5 $ Translate (1150) (50) $ desenhaVida (vidaBase base)
                piccreditos = Scale 0.5 0.5 $ Translate 1250 (-550) $ desenhaCreditos (creditosBase base)
                inimigoeste = getImagem "inimigoeste" limagens
                torres = filter (\(s,p) -> s == "torrefogo" || s == "torreresina" || s == "torregelo") limagens
                portal = getImagem "portal" limagens


desenhaVida :: Float -> Picture
desenhaVida v = Pictures [Color red $ translate 0 0 $ Text (show v)]

desenhaCreditos :: Int -> Picture
desenhaCreditos c = Pictures [Color (orange) $ translate 0 (-20) $ Text (show c)]

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

desenhaInimigo :: Picture -> Posicao -> Picture
desenhaInimigo inimigo (x,y) = translate (x * 40) (y * 40) $ Scale 0.1 0.1 $ inimigo

desenhaPortais :: Picture -> Posicao -> Picture
desenhaPortais portal (x,y) = Translate (x * 40) (y * 40) $ Scale 0.11 0.11 $ portal

desenhaTorres :: [(String,Picture)] -> (Posicao, TipoProjetil) -> Picture
desenhaTorres torres ((x,y), Fogo) = desenhaTorreFogo (getImagem "torrefogo" torres) (x,y)
desenhaTorres torres ((x,y), Resina) = desenhaTorreResina (getImagem "torreresina" torres) (x,y)
desenhaTorres torres ((x,y), Gelo) = desenhaTorreGelo (getImagem "torregelo" torres) (x,y)

desenhaTorreFogo :: Picture -> Posicao -> Picture
desenhaTorreFogo torrefogo (x, y) = Pictures
    [Translate (x * 40) (y * 40) $ Scale 0.11 0.11 $ torrefogo]

desenhaTorreResina :: Picture -> Posicao -> Picture
desenhaTorreResina torreresina (x, y) = Pictures
    [Translate (x * 40) (y * 40) $ Scale 0.11 0.11 $ torreresina]

desenhaTorreGelo :: Picture -> Posicao -> Picture
desenhaTorreGelo torregelo (x, y) = Pictures
    [Translate (x * 40) (y * 40) $ Scale 0.11 0.11 $ torregelo]

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