module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425

-- | Janela do jogo
janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

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
  play janela fundo fr (ImmutableTowers jogoInicio jogoInicio (MenuInicial Jogar) imagens Nothing Nothing) desenha reageEventos reageTempo

{-| Função que carrega as imagens do jogo de modo a passar estas para Picture e armazena-las
 numa lista de tuplas com a string igual o nome da imagem e a imagem em si, para facilitar a busca pela imagem correta nas outras funções -}

imagensLoad :: IO [(String, Picture)]
imagensLoad = do
    menujogar <- loadBMP "imgs/menujogar.bmp"
    menusair <- loadBMP "imgs/menusair.bmp"
    menuganhou <- loadBMP "imgs/ganhou.bmp"
    menuperdeu <- loadBMP "imgs/perdeu.bmp"
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
           ("menusair", menusair), 
           ("menuganhou", menuganhou), 
           ("menuperdeu", menuperdeu), 
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


-- | Estado do jogo inicial
jogoInicio :: Jogo
jogoInicio = Jogo {
    baseJogo = Base {
        vidaBase = 150,
        posicaoBase = (10.5, 5.5),
        creditosBase = 400
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (0.5, 4.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (0.5, 4.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 5,
                            butimInimigo = 25,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.5, 4.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 90,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 5,
                            butimInimigo = 25,
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 2.0,
                    tempoOnda = 5.0,
                    entradaOnda = 5.0
                },
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (0.5, 4.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 250,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 200,
                            butimInimigo = 25,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.5, 4.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 150,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 5,
                            butimInimigo = 25,
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 1.0,
                    tempoOnda = 5.0,
                    entradaOnda = 5.0
                }
            ]
        },
        Portal {
            posicaoPortal = (0.5, 6.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (0.5, 6.5),
                            direcaoInimigo = Norte,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 5,
                            butimInimigo = 25,
                            projeteisInimigo = []
                        },
                        Inimigo {
                            posicaoInimigo = (0.5, 6.5),
                            direcaoInimigo = Norte,
                            vidaInimigo = 100,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 5,
                            butimInimigo = 25,
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 2.0,
                    tempoOnda = 5.0,
                    entradaOnda = 5.0
                }]}
    ],
    torresJogo = [
    ],
    mapaJogo = mapa01,
    inimigosJogo = [],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 30,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 3,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 2.0
            },
            nivelTorre = 1
        }),
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 30,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 3,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 2.0
            },
            nivelTorre = 1
        }),
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 30,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 3,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Resina,
                duracaoProjetil = Finita 2.0
            },
            nivelTorre = 1
        })
    ],
    precoUpgrades = [
        (50, 1, Gelo),
        (50,1,Fogo),
        (50,1,Resina),
        (75,2,Gelo),
        (75,2,Fogo),
        (75,2,Resina),
        (100,3,Gelo),
        (100,3,Fogo),
        (100,3,Resina)
    ]
}