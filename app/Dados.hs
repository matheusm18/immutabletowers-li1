module Dados where

import LI12425
import Desenhar(mapa01,mapa02,mapa03)

-- | Estado do jogo inicial 1
jogoInicio01 :: Jogo
jogoInicio01 = Jogo {
    baseJogo = Base {
        vidaBase = 100,
        posicaoBase = (10.5, 8.5),
        creditosBase = 100
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (0.5, 2.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = replicate 2 inimJogo1Fase1Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 5.0,
                    entradaOnda = 10.0
                },
                Onda {
                    inimigosOnda = replicate 4 inimJogo1Fase1Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimJogo1Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = [
                    ],
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = [
                    ],
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = [
                    ],
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = [
                    ],
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = [
                    ],
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                }
            ]
        }],
    torresJogo = [],
    mapaJogo = mapa01,
    inimigosJogo = [],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 30,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 4,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 1.5
            },
            nivelTorre = 1
        }),
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 30,
            alcanceTorre = 1.5,
            rajadaTorre = 5,
            cicloTorre = 4,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 2.0
            },
            nivelTorre = 1
        }),
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 10,
            alcanceTorre = 2.1,
            rajadaTorre = 3,
            cicloTorre = 2,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Resina,
                duracaoProjetil = Finita 2.1
            },
            nivelTorre = 1
        })
    ],
    precoUpgrades = [
        (50,1, Gelo),
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

-- | Estado do jogo inicial 2
jogoInicio02 :: Jogo
jogoInicio02 = Jogo {
    baseJogo = Base {
        vidaBase = 100,
        posicaoBase = (5.5, 5.5),
        creditosBase = 200
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (1.5, 1.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = replicate 2 inimJogo2Fase1Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 5.0,
                    entradaOnda = 10.0
                },
                Onda {
                    inimigosOnda = replicate 4 inimJogo2Fase1Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimJogo2Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimJogo2Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = replicate 6 inimJogo2Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 6 inimJogo2Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = replicate 20 inimJogo2Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 20 inimJogo2Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                }
            ]
        },
        Portal {
            posicaoPortal = (9.5, 9.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = replicate 2 inimJogo2Fase1Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 5.0,
                    entradaOnda = 10.0
                },                
                Onda {
                    inimigosOnda = replicate 4 inimJogo2Fase1Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimJogo2Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimJogo2Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = replicate 6 inimJogo2Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 6 inimJogo2Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = replicate 20 inimJogo2Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 20 inimJogo2Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                }
                ]}    
    ],
    torresJogo = [],
    mapaJogo = mapa02,
    inimigosJogo = [],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 30,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 4,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 1.5
            },
            nivelTorre = 1
        }),
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 30,
            alcanceTorre = 1.5,
            rajadaTorre = 5,
            cicloTorre = 4,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 2.0
            },
            nivelTorre = 1
        }),
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 10,
            alcanceTorre = 2.1,
            rajadaTorre = 3,
            cicloTorre = 2,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Resina,
                duracaoProjetil = Finita 2.1
            },
            nivelTorre = 1
        })
    ],
    precoUpgrades = [
        (50,1, Gelo),
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

-- | Estado do jogo inicial 3
jogoInicio03 :: Jogo
jogoInicio03 = Jogo {
    baseJogo = Base {
        vidaBase = 100,
        posicaoBase = (10.5, 5.5),
        creditosBase = 200
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (0.5, 4.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = replicate 2 inimJogo3Fase1Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 5.0,
                    entradaOnda = 10.0
                },
                Onda {
                    inimigosOnda = replicate 4 inimJogo3Fase1Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimJogo3Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimJogo3Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = replicate 6 inimJogo3Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 6 inimJogo3Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = replicate 20 inimJogo3Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 20 inimJogo3Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                }
            ]
        },
        Portal {
            posicaoPortal = (0.5, 6.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = replicate 2 inimJogo3Fase1Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 5.0,
                    entradaOnda = 10.0
                },                
                Onda {
                    inimigosOnda = replicate 4 inimJogo3Fase1Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimJogo3Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimJogo3Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = replicate 6 inimJogo3Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 6 inimJogo3Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = replicate 20 inimJogo3Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 20 inimJogo3Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                }
                ]}
    ],
    torresJogo = [],
    mapaJogo = mapa03,
    inimigosJogo = [],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 30,
            alcanceTorre = 1.5,
            rajadaTorre = 2,
            cicloTorre = 4,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Gelo,
                duracaoProjetil = Finita 1.5
            },
            nivelTorre = 1
        }),
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 30,
            alcanceTorre = 1.5,
            rajadaTorre = 5,
            cicloTorre = 4,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Fogo,
                duracaoProjetil = Finita 2.0
            },
            nivelTorre = 1
        }),
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 10,
            alcanceTorre = 2.1,
            rajadaTorre = 3,
            cicloTorre = 2,
            tempoTorre = 0,
            projetilTorre = Projetil {
                tipoProjetil = Resina,
                duracaoProjetil = Finita 2.1
            },
            nivelTorre = 1
        })
    ],
    precoUpgrades = [
        (50,1, Gelo),
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

-- | Inimigos para o jogo1:

inimJogo1Fase1Portal1 :: Inimigo
inimJogo1Fase1Portal1 = Inimigo {posicaoInimigo = (0.5, 2.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 100,
                          velocidadeInimigo = 1,
                          ataqueInimigo = 10,
                          butimInimigo = 25,
                          projeteisInimigo = [],
                          tipoInimigo = Normal
                         }

inimJogo1Fase2Portal1 :: Inimigo
inimJogo1Fase2Portal1 = Inimigo {posicaoInimigo = (0.5, 2.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 150,
                          velocidadeInimigo = 1,
                          ataqueInimigo = 10,
                          butimInimigo = 25,
                          projeteisInimigo = [],
                          tipoInimigo = Normal
                         }

-- | Inimigos para o jogo2:

inimJogo2Fase1Portal1 :: Inimigo
inimJogo2Fase1Portal1 = inimJogo1Fase1Portal1 {posicaoInimigo = (1.5, 1.5)}

inimJogo2Fase2Portal1 :: Inimigo
inimJogo2Fase2Portal1 = inimJogo1Fase2Portal1 {posicaoInimigo = (1.5, 1.5)}

inimJogo2Fase1Portal2 :: Inimigo
inimJogo2Fase1Portal2 = Inimigo {posicaoInimigo = (9.5, 9.5),
                          direcaoInimigo = Oeste,
                          vidaInimigo = 100,
                          velocidadeInimigo = 1,
                          ataqueInimigo = 10,
                          butimInimigo = 25,
                          projeteisInimigo = [],
                          tipoInimigo = Normal
                         }

inimJogo2Fase2Portal2 :: Inimigo
inimJogo2Fase2Portal2 = Inimigo {posicaoInimigo = (9.5, 9.5),
                          direcaoInimigo = Oeste,
                          vidaInimigo = 150,
                          velocidadeInimigo = 1,
                          ataqueInimigo = 10,
                          butimInimigo = 25,
                          projeteisInimigo = [],
                          tipoInimigo = Blindado
                         }


-- | Inimigos para o jogo3:

inimJogo3Fase1Portal1 :: Inimigo
inimJogo3Fase1Portal1 = Inimigo {posicaoInimigo = (0.5, 4.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 100,
                          velocidadeInimigo = 1,
                          ataqueInimigo = 10,
                          butimInimigo = 25,
                          projeteisInimigo = [],
                          tipoInimigo = Normal
                         }

inimJogo3Fase2Portal1 :: Inimigo
inimJogo3Fase2Portal1 = Inimigo {posicaoInimigo = (0.5, 4.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 150,
                          velocidadeInimigo = 1,
                          ataqueInimigo = 10,
                          butimInimigo = 25,
                          projeteisInimigo = [],
                          tipoInimigo = Normal
                         }

inimJogo3Fase1Portal2 :: Inimigo
inimJogo3Fase1Portal2 = Inimigo {posicaoInimigo = (0.5, 6.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 100,
                          velocidadeInimigo = 1,
                          ataqueInimigo = 10,
                          butimInimigo = 25,
                          projeteisInimigo = [],
                          tipoInimigo = Normal
                         }

inimJogo3Fase2Portal2 :: Inimigo
inimJogo3Fase2Portal2 = Inimigo {posicaoInimigo = (0.5, 6.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 150,
                          velocidadeInimigo = 1,
                          ataqueInimigo = 10,
                          butimInimigo = 25,
                          projeteisInimigo = [],
                          tipoInimigo = Blindado
                         }
