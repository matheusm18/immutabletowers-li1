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


main :: IO ()
main = do
  menujogar <- loadBMP "app/menujogar.bmp"
  menusair <- loadBMP "app/menusair.bmp"
  menuganhou <- loadBMP "app/ganhou.bmp"
  menuperdeu <- loadBMP "app/perdeu.bmp"
  play janela fundo fr (ImmutableTowers jogoInicio jogoInicio (MenuInicial Jogar) [menujogar,menusair,menuganhou,menuperdeu]) desenha reageEventos reageTempo

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
            danoTorre = 15,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 5,
            tempoTorre = 1,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 3.0
            }
        },
        Torre {
            posicaoTorre = (0.5, 1.5),
            danoTorre = 15,
            alcanceTorre = 2,
            rajadaTorre = 2,
            cicloTorre = 5,
            tempoTorre = 5,
            projetilTorre = Projetil {
                tipoProjetil = Resina,
                duracaoProjetil = Finita 3.0
            }
        }
    ],
    mapaJogo = mapa01,
    inimigosJogo = [
        Inimigo {
            posicaoInimigo = (0.5, 3.5),
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