module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import Dados

-- | Janela do jogo.
janela :: Display
janela = FullScreen

-- | Background, escolhemos a cor cinza mas acabará por não aparecer, dado que, substituimos depois o fundo por imagens na função desenha.
fundo :: Color
fundo = greyN 0.6

-- | Frames por segundo (fps).
fr :: Int
fr = 60

-- | Função main, é a função principal que inicia o jogo.
main :: IO ()
main = do
  imagens <- imagensLoad
  play janela fundo fr (ImmutableTowers jogoInicio1 (MenuInicial) imagens Nothing Nothing Nothing 1 1) desenha reageEventos reageTempo

{-| Função que carrega as imagens do jogo de modo a passar estas para Picture e armazena-las
 numa lista de tuplas. 
 
 A primeira componente da tupla é uma string com o nome da imagem e a segunda componente é a imagem em si, 
 para facilitar a busca pela imagem correta nas outras funções.
-}

imagensLoad :: IO [(String, Picture)]
imagensLoad = do
    menujogar <- loadBMP "imgs/menujogar.bmp"
    menupausa <- loadBMP "imgs/menupausa.bmp"
    menuganhou <- loadBMP "imgs/ganhou.bmp"
    menuperdeu <- loadBMP "imgs/perdeu.bmp"
    menuzerou <- loadBMP "imgs/menuzerou.bmp"
    menutexturas <- loadBMP "imgs/menutexturas.bmp"
    menuniveis1 <- loadBMP "imgs/menuniveis1.bmp"
    menuniveis2 <- loadBMP "imgs/menuniveis2.bmp"
    menuniveis3 <- loadBMP "imgs/menuniveis3.bmp"
    bgjogo <- loadBMP "imgs/bgjogo.bmp"
    bgjogogelo <- loadBMP "imgs/bgjogogelo.bmp"
    bgjogofogo <- loadBMP "imgs/bgjogofogo.bmp"
    bgjogoresina <- loadBMP "imgs/bgjogoresina.bmp"
    inimigoEste <- loadBMP "imgs/inimigoEste.bmp"
    inimigofogoEste <- loadBMP "imgs/inimigoFogoEste.bmp"
    inimigoresinaEste <- loadBMP "imgs/inimigoResinaEste.bmp"
    inimigogeloEste <- loadBMP "imgs/inimigoGeloEste.bmp"
    inimigoOeste <- loadBMP "imgs/inimigoOeste.bmp"
    inimigofogoOeste <- loadBMP "imgs/inimigoFogoOeste.bmp"
    inimigoresinaOeste <- loadBMP "imgs/inimigoResinaOeste.bmp"
    inimigogeloOeste <- loadBMP "imgs/inimigoGeloOeste.bmp"
    inimigoNorte <- loadBMP "imgs/inimigoNorte.bmp"
    inimigofogoNorte <- loadBMP "imgs/inimigoFogoNorte.bmp"
    inimigoresinaNorte <- loadBMP "imgs/inimigoResinaNorte.bmp"
    inimigogeloNorte <- loadBMP "imgs/inimigoGeloNorte.bmp"
    inimigoSul <- loadBMP "imgs/inimigoSul.bmp"
    inimigofogoSul <- loadBMP "imgs/inimigoFogoSul.bmp"
    inimigoresinaSul <- loadBMP "imgs/inimigoResinaSul.bmp"
    inimigogeloSul <- loadBMP "imgs/inimigoGeloSul.bmp"
    blindadoNorte <- loadBMP "imgs/blindadoNorte.bmp"
    blindadoSul <- loadBMP "imgs/blindadoSul.bmp"
    blindadoEste <- loadBMP "imgs/blindadoEste.bmp"
    blindadoOeste <- loadBMP "imgs/blindadoOeste.bmp"
    bossNorte <- loadBMP "imgs/bossNorte.bmp"
    bossSul <- loadBMP "imgs/bossSul.bmp"
    bossEste <- loadBMP "imgs/bossEste.bmp"
    bossOeste <- loadBMP "imgs/bossOeste.bmp"
    torrefogo <- loadBMP "imgs/TorreFogo.bmp"
    torreresina <- loadBMP "imgs/TorreResina.bmp"
    torregelo <- loadBMP "imgs/TorreGelo.bmp"
    portal <- loadBMP "imgs/portal.bmp"
    base <- loadBMP "imgs/base.bmp"
    base50vida <- loadBMP "imgs/base50vida.bmp"
    base25vida <- loadBMP "imgs/base25vida.bmp"
    terrenoagua1 <- loadBMP "imgs/terrenoagua1.bmp"
    terrenoagua2 <- loadBMP "imgs/terrenoagua2.bmp"
    terrenorelva1 <- loadBMP "imgs/terrenorelva1.bmp"
    terrenorelva2 <- loadBMP "imgs/terrenorelva2.bmp"
    terrenoterra1 <- loadBMP "imgs/terrenoterra1.bmp"
    terrenoterra2 <- loadBMP "imgs/terrenoterra2.bmp"
    melhoriaGelo1 <- loadBMP "imgs/melhoriagelo1.bmp"
    melhoriaGelo2 <- loadBMP "imgs/melhoriagelo2.bmp"
    melhoriaGelo3 <- loadBMP "imgs/melhoriagelo3.bmp"
    melhoriaGelo4 <- loadBMP "imgs/melhoriagelo4.bmp"
    melhoriaFogo1 <- loadBMP "imgs/melhoriafogo1.bmp"
    melhoriaFogo2 <- loadBMP "imgs/melhoriafogo2.bmp"
    melhoriaFogo3 <- loadBMP "imgs/melhoriafogo3.bmp"
    melhoriaFogo4 <- loadBMP "imgs/melhoriafogo4.bmp"
    melhoriaResina1 <- loadBMP "imgs/melhoriaresina1.bmp"
    melhoriaResina2 <- loadBMP "imgs/melhoriaresina2.bmp"
    melhoriaResina3 <- loadBMP "imgs/melhoriaresina3.bmp"
    melhoriaResina4 <- loadBMP "imgs/melhoriaresina4.bmp"
    return [("menujogar", menujogar),
           ("menupausa", menupausa),
           ("menuganhou", menuganhou), 
           ("menuperdeu", menuperdeu),
           ("menuzerou", menuzerou),
           ("menutexturas", menutexturas),
           ("menuniveis1", menuniveis1),
           ("menuniveis2", menuniveis2),
           ("menuniveis3", menuniveis3),
           ("bgjogo", bgjogo),
           ("bgjogogelo", bgjogogelo),
           ("bgjogofogo", bgjogofogo),
           ("bgjogoresina", bgjogoresina),
           ("inimigoEste", inimigoEste),
           ("inimigofogoEste", inimigofogoEste),
           ("inimigoresinaEste", inimigoresinaEste),
           ("inimigogeloEste", inimigogeloEste),
           ("inimigoOeste", inimigoOeste),
           ("inimigofogoOeste", inimigofogoOeste),
           ("inimigoresinaOeste", inimigoresinaOeste),
           ("inimigogeloOeste", inimigogeloOeste),
           ("inimigoNorte", inimigoNorte),
           ("inimigofogoNorte", inimigofogoNorte),
           ("inimigoresinaNorte", inimigoresinaNorte),
           ("inimigogeloNorte", inimigogeloNorte),
           ("inimigoSul", inimigoSul),
           ("inimigofogoSul", inimigofogoSul),
           ("inimigoresinaSul", inimigoresinaSul),
           ("inimigogeloSul", inimigogeloSul),
           ("blindadoNorte", blindadoNorte),
           ("blindadoSul", blindadoSul),
           ("blindadoEste", blindadoEste),
           ("blindadoOeste", blindadoOeste),
           ("bossNorte", bossNorte),
           ("bossSul", bossSul),
           ("bossEste", bossEste),
           ("bossOeste", bossOeste),
           ("torrefogo", torrefogo),
           ("torreresina", torreresina),
           ("torregelo", torregelo),
           ("portal", portal),
           ("base",base),
           ("base50vida",base50vida),
           ("base25vida",base25vida),
           ("terrenoagua1", terrenoagua1),
           ("terrenoagua2", terrenoagua2),
           ("terrenorelva1", terrenorelva1),
           ("terrenorelva2", terrenorelva2),
           ("terrenoterra1", terrenoterra1),
           ("terrenoterra2", terrenoterra2),
           ("melhoriaGelo1", melhoriaGelo1),
           ("melhoriaGelo2", melhoriaGelo2),
           ("melhoriaGelo3", melhoriaGelo3),
           ("melhoriaGelo4", melhoriaGelo4),
           ("melhoriaFogo1", melhoriaFogo1),
           ("melhoriaFogo2", melhoriaFogo2),
           ("melhoriaFogo3", melhoriaFogo3),
           ("melhoriaFogo4", melhoriaFogo4),
           ("melhoriaResina1", melhoriaResina1),
           ("melhoriaResina2", melhoriaResina2),
           ("melhoriaResina3", melhoriaResina3),
           ("melhoriaResina4", melhoriaResina4)]