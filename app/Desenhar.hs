module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425
import Tarefa2(verificaGelo,verificaFogo)

-- | Função auxiliar que ajuda a converter as posições do jogo para as posições do gloss
invertePos :: Posicao -> Posicao
invertePos (x,y) = (x,-y)

-- | Função auxiliar que busca a imagem que esta associada a string na lista de imagens
getImagem :: String -> [(String,Picture)] -> Picture
getImagem s lista = case lookup s lista of
    Just p -> p
    Nothing -> Blank

-- | Função auxiliar que já retorna a posição do inimigo convertida para o gloss e a lista de projeteis do inimigo
getPosDirProjetil :: Inimigo -> (Posicao, Direcao, [Projetil])
getPosDirProjetil i = (invertePos (posicaoInimigo i),direcaoInimigo i, projeteisInimigo i)

-- | Função auxiliar que retorna a lista de imagens dos inimigos
getPicInimigos :: [(String,Picture)] -> [(String,Picture)]
getPicInimigos imagens = 
    filter (\(s,_) -> s == "inimigoNorte" || s == "inimigofogoNorte" || s == "inimigoresinaNorte" || s == "inimigogeloNorte" || 
                      s == "inimigoSul" || s == "inimigofogoSul" || s == "inimigoresinaSul" || s == "inimigogeloSul" ||
                      s == "inimigoEste" || s == "inimigofogoEste" || s == "inimigoresinaEste" || s == "inimigogeloEste" || 
                      s == "inimigoOeste" || s == "inimigofogoOeste" ||  s == "inimigoresinaOeste" || s == "inimigogeloOeste") imagens

-- | Função auxiliar que retorna a lista de imagens das melhorias das torres
getMelhoriasTorres :: [(String,Picture)] -> [(String,Picture)]
getMelhoriasTorres imagens = filter (\(s,_) -> s == "melhoriaGelo1" || s == "melhoriaGelo2" || s == "melhoriaGelo3" || s == "melhoriaGelo4" ||
                                                s == "melhoriaFogo1" || s == "melhoriaFogo2" || s == "melhoriaFogo3" || s == "melhoriaFogo4" ||
                                                s == "melhoriaResina1" || s == "melhoriaResina2" || s == "melhoriaResina3" || s == "melhoriaResina4") imagens

-- | Função que retorna a imagem de fundo dependendo da torre selecionada
getBg :: Maybe Torre -> [(String,Picture)] -> Picture
getBg Nothing limagens = getImagem "bgjogo" limagens
getBg (Just t) limagens = case tipoProjetil (projetilTorre t) of
    Fogo -> getImagem "bgjogofogo" limagens
    Resina -> getImagem "bgjogoresina" limagens
    Gelo -> getImagem "bgjogogelo" limagens

-- | Função principal para desenhar o mapa
desenha :: ImmutableTowers -> Picture
desenha ImmutableTowers {menu = MenuInicial Jogar, imagens = limagens} = getImagem "menujogar" limagens
desenha ImmutableTowers {menu = MenuInicial Sair, imagens = limagens} = getImagem "menusair" limagens
desenha ImmutableTowers {menu = ModoJogo GanhouJogo, imagens = limagens} = getImagem "menuganhou" limagens
desenha ImmutableTowers {menu = ModoJogo PerdeuJogo, imagens = limagens} = getImagem "menuperdeu" limagens
desenha (ImmutableTowers _ Jogo {baseJogo = base, portaisJogo = lportais, torresJogo = ltorres, inimigosJogo = linimigos, mapaJogo = mapa} (ModoJogo EmAndamento) limagens torreSelecionada infoTorre)
            =  Pictures [getBg torreSelecionada limagens,
                       (Translate (-440) (385) $ Pictures $ [desenhaMapa picTerrenos mapa] ++ [desenhaBase picBase posbase] ++ (map (desenhaTorres picTorres) lpostiposTorres) ++ (map (desenhaPortais picPortal) lposportais) ++ (map (desenhaInimigo picInimigos) dadosInimigos)),
                         picVida,
                         picCreditos,
                         desenhaInfoTorre picMelhoriasTorres infoTorre]
            where
                dadosInimigos = map getPosDirProjetil linimigos
                lposportais = map (\i -> invertePos(posicaoPortal i)) lportais
                (lpostorres, ltipoprojtorres) = (map (\t -> invertePos(posicaoTorre t)) ltorres, map (\p -> tipoProjetil p) (map (\t -> projetilTorre t) ltorres))
                lpostiposTorres = zip lpostorres ltipoprojtorres
                posbase = invertePos(posicaoBase base)
                picVida = desenhaVida (vidaBase base)
                picCreditos = desenhaCreditos (creditosBase base)
                picInimigos = getPicInimigos limagens
                picTorres = filter (\(s,_) -> s == "torrefogo" || s == "torreresina" || s == "torregelo") limagens
                picPortal = getImagem "portal" limagens
                picBase = getImagem "base" limagens
                picTerrenos = filter (\(s,_) -> s == "terrenoagua" || s == "terrenorelva" || s == "terrenoterra") limagens
                picMelhoriasTorres = getMelhoriasTorres limagens

-- | Função que zipa 3 listas em tuplas
zipThree :: [a] -> [b] -> [c] -> [(a,b,c)]
zipThree [] _ _ = []
zipThree _ [] _ = []
zipThree _ _ [] = []
zipThree (x:xs) (y:ys) (z:zs) = (x,y,z) : zipThree xs ys zs

-- | Função que desenha a vida da base
desenhaVida :: Float -> Picture
desenhaVida v = Color red $ Scale 0.5 0.5 $ translate (1275) (-175) $ Text (show v)

-- | Função que desenha os créditos da base
desenhaCreditos :: Int -> Picture
desenhaCreditos c = Pictures [Color (orange) $ Scale 0.5 0.5 $ translate 1325 (-675) $ Text (show c)]

{-| Função que recebe a lista de imagens e o mapa e retorna a imagem do mapa

Fazemos zip com a lista [0, -1..] porque o eixo do gloss cresce pra cima), então decidimos passar as coordenadas do y simétricas para o gloss

Ou seja, a segunda linha da matriz do mapa irá corresponder a lista das posições cujo y = -2 no gloss (no jogo seria y = 2)  -}

desenhaMapa :: [(String,Picture)] -> [[Terreno]] -> Picture
desenhaMapa limagens mapa = Pictures $ concatMap (desenhaLinha limagens) (zip [0,-1..] mapa)

-- | Função que dada a lista de imagens e uma tupla que contem a coordenada do y (já correspondente ao gloss) e a linha da matriz do mapa retorna uma lista de Picture
desenhaLinha :: [(String,Picture)] -> (Int, [Terreno]) -> [Picture]
desenhaLinha limagens (y, linha) = concatMap (desenhaChao limagens y) (zip [0..] linha)

-- | Função que dada a lista de imagens, a coordenada do y e uma tupla que contem a coordenada do x e o terreno retorna uma lista de Picture
desenhaChao :: [(String,Picture)] -> Int -> (Int, Terreno) -> [Picture]
desenhaChao limagens y (x, terreno) = [desenhaTerreno limagens terreno (posicaoCentro x y)]

-- | Calcula a posição do centro de um quadrado no mapa (na parte do y subtrai porque agora no eixo do gloss os valores de y são negativos)
posicaoCentro :: Int -> Int -> (Float, Float)
posicaoCentro x y = (fromIntegral x + 0.5, fromIntegral y - 0.5)

-- | Função para desenhar cada tipo de terreno (2/9 visto que a imagem é 360 pixeis e queremos passar para 80)
desenhaTerreno :: [(String,Picture)] -> Terreno -> Posicao -> Picture
desenhaTerreno limagens Agua (x, y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ (getImagem "terrenoagua" limagens)
desenhaTerreno limagens Relva (x, y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ (getImagem "terrenorelva" limagens)
desenhaTerreno limagens Terra (x, y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ (getImagem "terrenoterra" limagens)

-- | Largura de cada quadrado
w :: Float
w = 80

-- | Altura de cada quadrado
h :: Float
h = 80

-- | Função que desenha o inimigo de acordo com a imagem que ele deve ter (depende dos projéteis ativos)
desenhaInimigo :: [(String,Picture)] -> (Posicao,Direcao,[Projetil]) -> Picture
desenhaInimigo picinimigos ((x,y),dir,lp) =
    let inimigoGelo = getImagem ("inimigogelo" ++ (show dir)) picinimigos
        inimigoFogo = getImagem ("inimigofogo" ++ (show dir)) picinimigos
        inimigoResina = getImagem ("inimigoresina" ++ (show dir)) picinimigos
        inimigo = getImagem ("inimigo" ++ (show dir)) picinimigos
    in case lp of 
        [] -> desenhaInimigoAux inimigo (x,y)
        _ -> if verificaGelo lp
             then desenhaInimigoAux inimigoGelo (x,y)
             else if verificaFogo lp
             then desenhaInimigoAux inimigoFogo (x,y)
             else desenhaInimigoAux inimigoResina (x,y)

-- | Função auxiliar para a desenhaInimigo que faz o translate (posicionar o inimigo na posição certa) e o scale da imagem (passar de 360 px para 80 px)
desenhaInimigoAux :: Picture -> Posicao -> Picture
desenhaInimigoAux pic (x,y) = Translate (x * w) (y * h) $ Scale (2/9) (2/9) $ pic

-- | Função que desenha os portais. 
desenhaPortais :: Picture -> Posicao -> Picture
desenhaPortais portal (x,y) = Translate (x * w) (y * h) $ Scale (2/9) (2/9) $ portal

-- | Função que desenha as torres de acordo com o tipo de projétil que elas possuem
desenhaTorres :: [(String,Picture)] -> (Posicao, TipoProjetil) -> Picture
desenhaTorres torres ((x,y), Fogo) = Pictures [desenhaTorreFogo (getImagem "torrefogo" torres) (x,y)]
desenhaTorres torres ((x,y), Resina) = Pictures [desenhaTorreResina (getImagem "torreresina" torres) (x,y)]
desenhaTorres torres ((x,y), Gelo) = Pictures [desenhaTorreGelo (getImagem "torregelo" torres) (x,y)]

-- | Função auxiliar que desenha a torre de fogo
desenhaTorreFogo :: Picture -> Posicao -> Picture
desenhaTorreFogo torrefogo (x, y) = Pictures
    [Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ torrefogo]

-- | Função auxiliar que desenha a torre de resina
desenhaTorreResina :: Picture -> Posicao -> Picture
desenhaTorreResina torreresina (x, y) = Pictures
    [Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ torreresina]

-- | Função auxiliar que desenha a torre de gelo
desenhaTorreGelo :: Picture -> Posicao -> Picture
desenhaTorreGelo torregelo (x, y) = Pictures
    [Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ torregelo]

-- | Função que desenha a base
desenhaBase :: Picture -> Posicao -> Picture
desenhaBase base (x,y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ base

-- | Função que desenha as informações da torre (alcance e imagem da melhoria)
desenhaInfoTorre :: [(String,Picture)] -> Maybe Torre -> Picture
desenhaInfoTorre _ Nothing = Blank
desenhaInfoTorre picsMelhoriasTorres (Just (Torre {posicaoTorre = (x,y), alcanceTorre = alcance, projetilTorre = projetil, nivelTorre = nivel})) 
    = Translate (-440) 385 $ Pictures [Translate (1125) (-150) $ imagem, Color (makeColor 0.7 0.7 0.7 0.4) $ Translate (x * w) (-y * w) $ circleSolid (w*alcance)]
    where
        tipo = tipoProjetil projetil
        imagem = getImagem ("melhoria" ++ (show tipo) ++ (show nivel)) picsMelhoriasTorres

-- | Mapa do jogo
mapa01 :: Mapa
mapa01 =
    [ [a,a,a,a,a,a,a,a,a,a,a],
      [r,r,r,r,r,r,r,r,r,a,a],
      [r,t,t,t,t,t,t,t,r,a,a],
      [r,t,r,r,r,r,r,t,r,r,r],
      [t,t,a,a,a,a,a,t,t,t,r],
      [a,a,a,a,a,a,a,a,r,t,t],
      [t,t,a,a,a,a,a,t,t,t,r],
      [r,t,r,r,r,r,r,t,r,r,r],
      [r,t,t,t,t,t,t,t,r,a,a],
      [r,r,r,r,r,r,r,r,r,a,a],
      [a,a,a,a,a,a,a,a,a,a,a]
    ]
  where
    t = Terra
    r = Relva
    a = Agua