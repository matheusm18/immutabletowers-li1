module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import Tempo
import LI12425
import Tarefa1
import Tarefa2
import Tarefa3

jogoInicio :: Jogo
jogoInicio = Jogo {
    baseJogo = Base {
        vidaBase = 100,  -- Exemplo de vida inicial da base
        posicaoBase = (5.5, 2.5),  -- Posição da base no mapa
        creditosBase = 50  -- Exemplo de créditos iniciais
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (0.5, 0.5),  -- Posição do primeiro portal
            ondasPortal = [
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (1.5, 1.5),
                            direcaoInimigo = Sul,
                            vidaInimigo = 50,  -- Exemplo de vida inicial do inimigo
                            velocidadeInimigo = 1,  -- Velocidade do inimigo
                            ataqueInimigo = 5,  -- Dano do inimigo na base
                            butimInimigo = 10,  -- Créditos que o jogador recebe
                            projeteisInimigo = []
                        }
                    ],
                    cicloOnda = 2.0,  -- Tempo entre inimigos da onda
                    tempoOnda = 2.0,  -- Tempo restante até o próximo inimigo
                    entradaOnda = 5.0  -- Tempo até a entrada da onda
                }
            ]
        }
    ],
    torresJogo = [
        Torre {
            posicaoTorre = (4.5, 3.5),
            danoTorre = 20,  -- Dano da torre
            alcanceTorre = 3.0,  -- Alcance da torre
            rajadaTorre = 1,  -- Número máximo de inimigos atingidos por rajada
            cicloTorre = 2.0,  -- Tempo entre rajadas
            tempoTorre = 2.0,  -- Tempo restante até a próxima rajada
            projetilTorre = Projetil {
                tipoProjetil = Resina,  -- Tipo de projétil da torre
                duracaoProjetil = Finita 3.0  -- Duração do efeito do projétil
            }
        },
        Torre {
            posicaoTorre = (0.5, 2.5),
            danoTorre = 15,
            alcanceTorre = 4.0,
            rajadaTorre = 2,
            cicloTorre = 1.5,
            tempoTorre = 1.5,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 5.0
            }
        }
    ],
    mapaJogo = mapa01,  -- Aqui você pode definir o seu mapa, por exemplo, um mapa de exemplo
    inimigosJogo = [
        Inimigo {
            posicaoInimigo = (2.5, 4.5),
            direcaoInimigo = Este,
            vidaInimigo = 100,
            velocidadeInimigo = 1,
            ataqueInimigo = 10,
            butimInimigo = 15,
            projeteisInimigo = []
        }
    ],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0, 0),  -- A posição não importa aqui, pois será definida na construção
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
fr = 10

main :: IO ()
main = do
  play janela fundo fr it desenha reageEventos reageTempo
  where
    it = ImmutableTowers jogoInicio