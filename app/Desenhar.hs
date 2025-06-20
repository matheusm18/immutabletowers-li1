module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425
import Tarefa2(verificaGelo,verificaFogo)

-- | Função auxiliar que transforma a posição para tornar o y simétrico (ajuda a converter as posições do jogo para as posições do gloss).
invertePos :: Posicao -> Posicao
invertePos (x,y) = (x,-y)

-- | Função auxiliar que busca a imagem que esta associada a string na lista de imagens.
getImagem :: String -> [(String,Picture)] -> Picture
getImagem s lista = case lookup s lista of
    Just p -> p
    Nothing -> Blank

-- | Função auxiliar que retorna a tupla com a posição do inimigo convertida para o gloss, a direção, a lista de projeteis do inimigo e o tipo do inimigo.
getPosDirProjetil :: Inimigo -> (Posicao, Direcao, [Projetil], TipoInimigo)
getPosDirProjetil i = (invertePos (posicaoInimigo i),direcaoInimigo i, projeteisInimigo i, tipoInimigo i)

-- | Função auxiliar que retorna a lista de imagens dos inimigos.
getPicInimigos :: [(String,Picture)] -> [(String,Picture)]
getPicInimigos limagens = 
    filter (\(s,_) -> s == "inimigoNorte" || s == "inimigofogoNorte" || s == "inimigoresinaNorte" || s == "inimigogeloNorte" || 
                      s == "inimigoSul" || s == "inimigofogoSul" || s == "inimigoresinaSul" || s == "inimigogeloSul" ||
                      s == "inimigoEste" || s == "inimigofogoEste" || s == "inimigoresinaEste" || s == "inimigogeloEste" || 
                      s == "inimigoOeste" || s == "inimigofogoOeste" ||  s == "inimigoresinaOeste" || s == "inimigogeloOeste" ||
                      s == "blindadoNorte" || s == "blindadoSul" || s == "blindadoEste" || s == "blindadoOeste" ||
                      s == "bossNorte" || s == "bossSul" || s == "bossEste" || s == "bossOeste") limagens

-- | Função auxiliar que retorna a lista de imagens das melhorias das torres.
getMelhoriasTorres :: [(String,Picture)] -> [(String,Picture)]
getMelhoriasTorres limagens = filter (\(s,_) -> s == "melhoriaGelo1" || s == "melhoriaGelo2" || s == "melhoriaGelo3" || s == "melhoriaGelo4" ||
                                                s == "melhoriaFogo1" || s == "melhoriaFogo2" || s == "melhoriaFogo3" || s == "melhoriaFogo4" ||
                                                s == "melhoriaResina1" || s == "melhoriaResina2" || s == "melhoriaResina3" || s == "melhoriaResina4") limagens

-- | Função auxiliar que retorna a lista de imagens dos terrenos dependendo da textura selecionada.
getPicTerrenos :: Int -> [(String,Picture)] -> [(String,Picture)]
getPicTerrenos textura limagens = 
  case textura of 
    1 -> filter (\(s,_) -> s == "terrenoagua1" || s == "terrenorelva1" || s == "terrenoterra1") limagens
    _ -> filter (\(s,_) -> s == "terrenoagua2" || s == "terrenorelva2" || s == "terrenoterra2") limagens

-- | Função auxiliar que retorna a Picture da base dependendo da vida dela.
getPicBase :: Base -> [(String,Picture)] -> Picture
getPicBase base limagens = if (vidaBase base) <= 25.0 then getImagem "base25vida" limagens
                        else if (vidaBase base) <= 50.0 then getImagem "base50vida" limagens
                        else getImagem "base" limagens

-- | Função que retorna a imagem de fundo dependendo da torre selecionada.
getBg :: Maybe Torre -> [(String,Picture)] -> Picture
getBg Nothing limagens = getImagem "bgjogo" limagens
getBg (Just t) limagens = case tipoProjetil (projetilTorre t) of
    Fogo -> getImagem "bgjogofogo" limagens
    Resina -> getImagem "bgjogoresina" limagens
    Gelo -> getImagem "bgjogogelo" limagens

-- | Função principal para desenhar o jogo.
desenha :: ImmutableTowers -> Picture
desenha ImmutableTowers {menu = MenuInicial, imagens = limagens} = getImagem "menujogar" limagens
desenha it@(ImmutableTowers {menu = ModoJogo EscolherNivel, imagens = limagens}) 
  = case nivelMaximo it of 
      1 -> getImagem "menuniveis1" limagens
      2 -> getImagem "menuniveis2" limagens
      _ -> getImagem "menuniveis3" limagens
desenha ImmutableTowers {menu = ModoJogo Texturas, imagens = limagens} = getImagem "menutexturas" limagens
desenha it@(ImmutableTowers {menu = ModoJogo GanhouJogo, imagens = limagens})
  = if nivelAtual it == Just 3 && nivelMaximo it == 3 then getImagem "menuzerou" limagens else getImagem "menuganhou" limagens
desenha ImmutableTowers {menu = ModoJogo PerdeuJogo, imagens = limagens} = getImagem "menuperdeu" limagens
desenha ImmutableTowers {menu = ModoJogo Pausa, imagens = limagens} = getImagem "menupausa" limagens
desenha (ImmutableTowers Jogo {baseJogo = base, portaisJogo = lportais, torresJogo = ltorres, inimigosJogo = linimigos, mapaJogo = mapa} (ModoJogo EmAndamento) limagens torreSelecionada infoTorre _ _ textura)
            =  Pictures [getBg torreSelecionada limagens,
                        (Translate (-440) (385) $ Pictures $ [desenhaMapa textura picTerrenos mapa] ++ [desenhaBase picBase posbase] ++ (map (desenhaTorres picTorres) lpostiposTorres) ++ (map (desenhaPortais picPortal) lposportais) ++ (map (desenhaInimigo picInimigos) dadosInimigos)),
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
                picTerrenos = getPicTerrenos textura limagens
                picPortal = getImagem "portal" limagens
                picBase = getPicBase base limagens
                picMelhoriasTorres = getMelhoriasTorres limagens

-- | Função que desenha a vida da base.
desenhaVida :: Float -> Picture
desenhaVida v = Color red $ Scale 0.5 0.5 $ translate (1325) (-175) $ Text (show (round v))

-- | Função que desenha os créditos da base.
desenhaCreditos :: Int -> Picture
desenhaCreditos c = Color (orange) $ Scale 0.5 0.5 $ translate 1325 (-675) $ Text (show c)

{-| Função que recebe o numero da textura atual, a lista de imagens e o mapa, de modo a retornar a imagem do mapa.

Fazemos zip com a lista [0, -1..] porque o eixo do gloss cresce pra cima), então decidimos passar as coordenadas do y simétricas para o gloss.

Ou seja, a segunda linha da matriz do mapa irá corresponder a lista das posições cujo y = -2 no gloss (no jogo seria y = 2).
-}

desenhaMapa :: Int -> [(String,Picture)] -> Mapa -> Picture
desenhaMapa textura limagens mapa = Pictures $ concatMap (desenhaLinha textura limagens) (zip [0,-1..] mapa)

{-| Função que dado o número da textura atual, a lista de imagens e uma tupla que contem a coordenada do y (já correspondente ao gloss) 
e a linha da matriz do mapa retorna uma lista de Picture.
-}

desenhaLinha :: Int -> [(String,Picture)] -> (Int, [Terreno]) -> [Picture]
desenhaLinha textura limagens (y, linha) = concatMap (desenhaChao textura limagens y) (zip [0..] linha)

{-| Função que dado o número da textura atual, a lista de imagens, a coordenada do y e uma tupla que contem a coordenada do x e o terreno 
retorna uma lista de Picture.
-}

desenhaChao :: Int -> [(String,Picture)] -> Int -> (Int, Terreno) -> [Picture]
desenhaChao textura limagens y (x, terreno) = [desenhaTerreno textura limagens terreno (posicaoCentro x y)]

-- | Calcula a posição do centro de um quadrado no mapa (na parte do y subtrai porque agora no eixo do gloss os valores de y são negativos).
posicaoCentro :: Int -> Int -> (Float, Float)
posicaoCentro x y = (fromIntegral x + 0.5, fromIntegral y - 0.5)

-- | Função para desenhar cada tipo de terreno (2/9 visto que a imagem é 360 pixeis e queremos passar para 80).
desenhaTerreno :: Int -> [(String,Picture)] -> Terreno -> Posicao -> Picture
desenhaTerreno textura limagens Agua (x, y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ (getImagem ("terrenoagua" ++ (show textura)) limagens)
desenhaTerreno textura limagens Relva (x, y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ (getImagem ("terrenorelva" ++ (show textura)) limagens)
desenhaTerreno textura limagens Terra (x, y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ (getImagem ("terrenoterra" ++ (show textura)) limagens)

-- | Largura de cada quadrado.
w :: Float
w = 80

-- | Altura de cada quadrado.
h :: Float
h = 80

-- | Função que desenha o inimigo de acordo com a imagem que ele deve ter (depende dos projéteis ativos).
desenhaInimigo :: [(String,Picture)] -> (Posicao,Direcao,[Projetil], TipoInimigo) -> Picture
desenhaInimigo picinimigos ((x,y),dir,lp, tipoInim) =
    let inimigoGelo = getImagem ("inimigogelo" ++ (show dir)) picinimigos
        inimigoFogo = getImagem ("inimigofogo" ++ (show dir)) picinimigos
        inimigoResina = getImagem ("inimigoresina" ++ (show dir)) picinimigos
        inimigo = getImagem ("inimigo" ++ (show dir)) picinimigos
        blindado = getImagem ("blindado" ++ (show dir)) picinimigos
        boss = getImagem ("boss" ++ (show dir)) picinimigos
    in if tipoInim == Normal then 
        case lp of 
         [] -> desenhaInimigoAux inimigo (x,y)
         _ -> if verificaGelo lp
              then desenhaInimigoAux inimigoGelo (x,y)
              else if verificaFogo lp
              then desenhaInimigoAux inimigoFogo (x,y)
              else desenhaInimigoAux inimigoResina (x,y)
       else if tipoInim == Blindado then desenhaInimigoAux blindado (x,y)
       else desenhaInimigoAux boss (x,y)

-- | Função auxiliar para a desenhaInimigo que faz o translate (posicionar o inimigo na posição certa) e o scale da imagem (passar de 360 px para 80 px).
desenhaInimigoAux :: Picture -> Posicao -> Picture
desenhaInimigoAux pic (x,y) = Translate (x * w) (y * h) $ Scale (2/9) (2/9) $ pic

-- | Função que desenha os portais. 
desenhaPortais :: Picture -> Posicao -> Picture
desenhaPortais portal (x,y) = Translate (x * w) (y * h) $ Scale (2/9) (2/9) $ portal

-- | Função que desenha as torres de acordo com o tipo de projétil que elas possuem.
desenhaTorres :: [(String,Picture)] -> (Posicao, TipoProjetil) -> Picture
desenhaTorres torres ((x,y), Fogo) = desenhaTorreFogo (getImagem "torrefogo" torres) (x,y)
desenhaTorres torres ((x,y), Resina) = desenhaTorreResina (getImagem "torreresina" torres) (x,y)
desenhaTorres torres ((x,y), Gelo) = desenhaTorreGelo (getImagem "torregelo" torres) (x,y)

-- | Função auxiliar que desenha a torre de fogo.
desenhaTorreFogo :: Picture -> Posicao -> Picture
desenhaTorreFogo torrefogo (x, y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ torrefogo

-- | Função auxiliar que desenha a torre de resina.
desenhaTorreResina :: Picture -> Posicao -> Picture
desenhaTorreResina torreresina (x, y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ torreresina

-- | Função auxiliar que desenha a torre de gelo.
desenhaTorreGelo :: Picture -> Posicao -> Picture
desenhaTorreGelo torregelo (x, y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ torregelo

-- | Função que desenha a base.
desenhaBase :: Picture -> Posicao -> Picture
desenhaBase base (x,y) = Translate (x * w) (y * w) $ Scale (2/9) (2/9) $ base

-- | Função que desenha as informações da torre (alcance e imagem da melhoria).
desenhaInfoTorre :: [(String,Picture)] -> Maybe Torre -> Picture
desenhaInfoTorre _ Nothing = Blank
desenhaInfoTorre picsMelhoriasTorres (Just (Torre {posicaoTorre = (x,y), alcanceTorre = alcance, projetilTorre = projetil, nivelTorre = nivel})) 
    = Translate (-440) 385 $ Pictures [Translate (1125) (-150) $ imagem, Color (makeColor 0.7 0.7 0.7 0.4) $ Translate (x * w) (-y * w) $ circleSolid (w*alcance)]
    where
        tipo = tipoProjetil projetil
        imagem = getImagem ("melhoria" ++ (show tipo) ++ (show nivel)) picsMelhoriasTorres

-- | Mapa do jogo (nível 1)
mapa01 :: Mapa
mapa01 =
    [ [a,a,a,a,a,a,a,a,a,a,a],
      [r,r,r,a,a,a,r,r,r,r,a],
      [t,t,t,r,a,a,r,t,t,t,r],
      [r,r,t,r,a,r,r,t,r,t,r],
      [a,r,t,r,r,r,t,t,r,t,r],
      [a,r,t,t,r,t,t,r,r,t,r],
      [a,r,r,t,r,t,r,r,t,t,r],
      [a,r,r,t,t,t,r,r,t,a,a],
      [a,a,r,r,r,r,r,r,t,t,t],
      [a,a,a,a,a,a,a,r,r,r,r],
      [a,a,a,a,a,a,a,a,a,a,a]
    ]
  where
    t = Terra
    r = Relva
    a = Agua

-- | Mapa do jogo (nível 2)
mapa02 :: Mapa
mapa02 =
    [ [a,a,a,r,r,r,r,r,a,a,a],
      [a,t,t,t,t,t,t,t,t,t,t],
      [a,a,a,a,r,r,r,a,a,a,t],
      [t,t,t,t,t,t,t,t,t,t,t],
      [t,a,r,a,a,r,a,a,r,a,a],
      [t,t,t,t,t,t,t,t,t,t,t],
      [a,a,r,a,a,r,a,a,r,a,t],
      [t,t,t,t,t,t,t,t,t,t,t],
      [t,a,a,a,r,r,r,a,a,a,a],
      [t,t,t,t,t,t,t,t,t,t,a],
      [a,a,a,r,r,r,r,r,a,a,a]
    ]
  where
    t = Terra
    r = Relva
    a = Agua

-- | Mapa do jogo (nível 3)
mapa03 :: Mapa
mapa03 =
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