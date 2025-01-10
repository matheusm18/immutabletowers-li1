module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import Dados

-- | Janela do jogo
janela :: Display
janela = FullScreen

-- | Background, escolhemos a cor cinza mas acabará por não aparecer, substituimos depois o fundo por imagens na função desenha
fundo :: Color
fundo = greyN 0.6

-- | Frames por segundo (fps)
fr :: Int
fr = 60

-- | Função main, é a função principal que inicia o jogo
main :: IO ()
main = do
  imagens <- imagensLoad
  play janela fundo fr (ImmutableTowers jogoInicio01 (MenuInicial) imagens Nothing Nothing 1) desenha reageEventos reageTempo

{-| Função que carrega as imagens do jogo de modo a passar estas para Picture e armazena-las
 numa lista de tuplas com a string igual o nome da imagem e a imagem em si, para facilitar a busca pela imagem correta nas outras funções -}

imagensLoad :: IO [(String, Picture)]
imagensLoad = do
    menujogar <- loadBMP "imgs/menujogar.bmp"
    menuganhou <- loadBMP "imgs/ganhou.bmp"
    menuperdeu <- loadBMP "imgs/perdeu.bmp"
    menuniveis1 <- loadBMP "imgs/menuniveis1.bmp"
    menuniveis2 <- loadBMP "imgs/menuniveis2.bmp"
    menuniveis3 <- loadBMP "imgs/menuniveis3.bmp"
    bgjogo <- loadBMP "imgs/bgjogo.bmp"
    bgjogogelo <- loadBMP "imgs/bgjogogelo.bmp"
    bgjogofogo <- loadBMP "imgs/bgjogofogo.bmp"
    bgjogoresina <- loadBMP "imgs/bgjogoresina.bmp"
    inimigoEste <- loadBMP "imgs/SoldadoEste.bmp"
    inimigofogoEste <- loadBMP "imgs/SoldadoFogoEste.bmp"
    inimigoresinaEste <- loadBMP "imgs/SoldadoResinaEste.bmp"
    inimigogeloEste <- loadBMP "imgs/SoldadoGeloEste.bmp"
    inimigoOeste <- loadBMP "imgs/SoldadoOeste.bmp"
    inimigofogoOeste <- loadBMP "imgs/SoldadoFogoOeste.bmp"
    inimigoresinaOeste <- loadBMP "imgs/SoldadoResinaOeste.bmp"
    inimigogeloOeste <- loadBMP "imgs/SoldadoGeloOeste.bmp"
    inimigoNorte <- loadBMP "imgs/SoldadoNorte.bmp"
    inimigofogoNorte <- loadBMP "imgs/SoldadoFogoNorte.bmp"
    inimigoresinaNorte <- loadBMP "imgs/SoldadoResinaNorte.bmp"
    inimigogeloNorte <- loadBMP "imgs/SoldadoGeloNorte.bmp"
    inimigoSul <- loadBMP "imgs/SoldadoSul.bmp"
    inimigofogoSul <- loadBMP "imgs/SoldadoFogoSul.bmp"
    inimigoresinaSul <- loadBMP "imgs/SoldadoResinaSul.bmp"
    inimigogeloSul <- loadBMP "imgs/SoldadoGeloSul.bmp"
    blindadoNorte <- loadBMP "imgs/blindadoNorte.bmp"
    blindadoSul <- loadBMP "imgs/blindadoSul.bmp"
    blindadoEste <- loadBMP "imgs/blindadoEste.bmp"
    blindadoOeste <- loadBMP "imgs/blindadoOeste.bmp"
    torrefogo <- loadBMP "imgs/TorreFogo.bmp"
    torreresina <- loadBMP "imgs/TorreResina.bmp"
    torregelo <- loadBMP "imgs/TorreGelo.bmp"
    portal <- loadBMP "imgs/portal.bmp"
    base <- loadBMP "imgs/base.bmp"
    terrenoagua <- loadBMP "imgs/terrenoagua.bmp"
    terrenorelva <- loadBMP "imgs/terrenorelva.bmp"
    terrenoterra <- loadBMP "imgs/terrenoterra.bmp"
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
           ("menuganhou", menuganhou), 
           ("menuperdeu", menuperdeu),
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
           ("torrefogo", torrefogo),
           ("torreresina", torreresina),
           ("torregelo", torregelo),
           ("portal", portal),
           ("base",base),
           ("terrenoagua", terrenoagua),
           ("terrenorelva", terrenorelva),
           ("terrenoterra", terrenoterra),
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