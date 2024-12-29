module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425


janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

fundo :: Color
fundo = greyN 0.6

fr :: Int
fr = 10

imagensLoad :: IO [(String, Picture)]
imagensLoad = do
    menujogar <- loadBMP "imgs/menujogar.bmp"
    menusair <- loadBMP "imgs/menusair.bmp"
    menuganhou <- loadBMP "imgs/ganhou.bmp"
    menuperdeu <- loadBMP "imgs/perdeu.bmp"
    bgjogo <- loadBMP "imgs/bgjogo.bmp"
    inimigoeste <- loadBMP "imgs/inimigoEste.bmp"
    inimigooeste <- loadBMP "imgs/inimigoOeste.bmp"
    torrefogo <- loadBMP "imgs/TorreFogo.bmp"
    torreresina <- loadBMP "imgs/TorreResina.bmp"
    torregelo <- loadBMP "imgs/TorreGelo.bmp"
    portal <- loadBMP "imgs/portal.bmp"
    return [("menujogar", menujogar), 
           ("menusair", menusair), 
           ("menuganhou", menuganhou), 
           ("menuperdeu", menuperdeu), 
           ("bgjogo", bgjogo), 
           ("inimigoeste", inimigoeste), 
           ("inimigooeste", inimigooeste),
           ("torrefogo", torrefogo),
           ("torreresina", torreresina),
           ("torregelo", torregelo),
           ("portal", portal)]

main :: IO ()
main = do
  imagens <- imagensLoad
  play janela fundo fr (ImmutableTowers jogoInicio jogoInicio (MenuInicial Jogar) imagens) desenha reageEventos reageTempo

jogoInicio :: Jogo
jogoInicio = Jogo {
    baseJogo = Base {
        vidaBase = 150,
        posicaoBase = (7.5, 1.5),
        creditosBase = 0
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (0.5, 0.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (1.5, 2.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 50,
                            velocidadeInimigo = 0.1,
                            ataqueInimigo = 5,
                            butimInimigo = 10,
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 2.0,
                    tempoOnda = 10.0,
                    entradaOnda = 1.0
                }
            ]
        }
    ],
    torresJogo = [
        Torre {
            posicaoTorre = (4.5, 3.5),
            danoTorre = 50,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 5,
            tempoTorre = 1,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 3.0
            }
        },
        Torre {
            posicaoTorre = (0.5, 1.5),
            danoTorre = 70,
            alcanceTorre = 2,
            rajadaTorre = 2,
            cicloTorre = 5,
            tempoTorre = 3,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 3.0
            }
        }
    ],
    mapaJogo = mapa01,
    inimigosJogo = [
        Inimigo {
            posicaoInimigo = (1.5, 3.5),
            direcaoInimigo = Sul,
            vidaInimigo = 100,
            velocidadeInimigo = 1,
            ataqueInimigo = 150,
            butimInimigo = 15,
            projeteisInimigo = []
        },
        Inimigo {
            posicaoInimigo = (0.5, 0.5),
            direcaoInimigo = Este,
            vidaInimigo = 100,
            velocidadeInimigo = 5,
            ataqueInimigo = 10,
            butimInimigo = 25,
            projeteisInimigo = []
        },
        Inimigo {
            posicaoInimigo = (1.5, 2.5),
            direcaoInimigo = Sul,
            vidaInimigo = 70,
            velocidadeInimigo = 5,
            ataqueInimigo = 10,
            butimInimigo = 25,
            projeteisInimigo = []
        },
        Inimigo {
            posicaoInimigo = (1.5, 1.5),
            direcaoInimigo = Sul,
            vidaInimigo = 100,
            velocidadeInimigo = 5,
            ataqueInimigo = 10,
            butimInimigo = 25,
            projeteisInimigo = []
        }
    ],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 25,
            alcanceTorre = 3.5,
            rajadaTorre = 3,
            cicloTorre = 1.0,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 2.0
            }
        })
    ]
}