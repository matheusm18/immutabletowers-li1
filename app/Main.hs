module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425

jogoInicio :: Jogo
jogoInicio = Jogo {
    baseJogo = Base {
        vidaBase = 100,
        posicaoBase = (5.5, 2.5),
        creditosBase = 50
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (0.5, 0.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (1.5, 1.5),
                            direcaoInimigo = Sul,
                            vidaInimigo = 50,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 5,
                            butimInimigo = 10,
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 2.0,
                    tempoOnda = 2.0,
                    entradaOnda = 5.0
                }
            ]
        }
    ],
    torresJogo = [
        Torre {
            posicaoTorre = (4.5, 3.5),
            danoTorre = 20,
            alcanceTorre = 3.0,
            rajadaTorre = 1,
            cicloTorre = 2.0,
            tempoTorre = 2.0,
            projetilTorre = Projetil {
                tipoProjetil = Resina,
                duracaoProjetil = Finita 3.0
            }
        },
        Torre {
            posicaoTorre = (0.5, 1.5),
            danoTorre = 50,
            alcanceTorre = 100,
            rajadaTorre = 5,
            cicloTorre = 1.5,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 5.0
            }
        }
    ],
    mapaJogo = mapa01,
    inimigosJogo = [
        Inimigo {
            posicaoInimigo = (0.5, 0.5),
            direcaoInimigo = Este,
            vidaInimigo = 100,
            velocidadeInimigo = 10,
            ataqueInimigo = 10,
            butimInimigo = 15,
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

janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

fundo :: Color
fundo = greyN 0.6

fr :: Int
fr = 5

main :: IO ()
main = do
  play janela fundo fr it desenha reageEventos reageTempo
  where
    it = ImmutableTowers jogoInicio