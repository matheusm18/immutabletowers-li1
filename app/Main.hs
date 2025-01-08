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

{-| Função que carrega as imagens do jogo de modo a estas estarem armazenadas numa lista de tuplas com a string igual o nome da imagem e a imagem em si, 
para facilita rdepois a busca pela imagem correta nas outras funções -}

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
    iconetorregelonivel2 <- loadBMP "imgs/iciconetorregelonivel1.bmp"
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
           ("iconetorregelonivel2", iconetorregelonivel2)]


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
                }]}
    ],
    torresJogo = [
    ],
    mapaJogo = mapa01,
    inimigosJogo = [],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 25,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 5,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 2.0
            },
            nivel = 1
        }),
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 25,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 5,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 2.0
            },
            nivel = 1
        }),
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 25,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 5,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Resina,
                duracaoProjetil = Finita 2.0
            },
            nivel = 1
        })
    ]
}