module Dados where

import LI12425
import Desenhar(mapa01,mapa02,mapa03)

-- | Estado do jogo inicial 1
jogoInicio1 :: Jogo
jogoInicio1 = Jogo {
    baseJogo = Base {
        vidaBase = 100,
        posicaoBase = (10.5, 8.5),
        creditosBase = 150
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (0.5, 2.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = replicate 2 inimJogo1Fase1Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 5.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 4 inimJogo1Fase1Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 10.0
                },
                Onda {
                    inimigosOnda = replicate 5 inimJogo1Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 10.0
                },
                Onda {
                    inimigosOnda = replicate 5 inimJogo1Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 5.0
                },
                Onda {
                    inimigosOnda = replicate 1 inimBlinJogo1Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 10 inimJogo1Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 5.0
                }
            ]
        }],
    torresJogo = [],
    mapaJogo = mapa01,
    inimigosJogo = [],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 35,
            alcanceTorre = 1.5,
            rajadaTorre = 4,
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
jogoInicio2 :: Jogo
jogoInicio2 = Jogo {
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
                    inimigosOnda = replicate 3 inimJogo2Fase2Portal1 ++ replicate 1 inimBlinJogo2Portal1 ++ replicate 3 inimJogo2Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 5 inimJogo2Fase2Portal1 ++ replicate 3 inimBlinJogo2Portal1 ++ replicate 5 inimJogo2Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 20.0
                },
                Onda {
                    inimigosOnda = replicate 5 inimBlinJogo2Portal1 ++ replicate 10 inimJogo2Fase2Portal1 ++ replicate 5 inimBlinJogo2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 20.0
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
                    inimigosOnda = replicate 3 inimJogo2Fase2Portal2 ++ replicate 1 inimBlinJogo2Portal2 ++ replicate 3 inimJogo2Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 5 inimJogo2Fase2Portal2 ++ replicate 3 inimBlinJogo2Portal2 ++ replicate 5 inimJogo2Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 20.0
                },
                Onda {
                    inimigosOnda = replicate 5 inimBlinJogo2Portal2 ++ replicate 10 inimJogo2Fase2Portal2 ++ replicate 5 inimBlinJogo2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 20.0
                }
                ]}    
    ],
    torresJogo = [],
    mapaJogo = mapa02,
    inimigosJogo = [],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 35,
            alcanceTorre = 1.5,
            rajadaTorre = 4,
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
jogoInicio3 :: Jogo
jogoInicio3 = Jogo {
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
                    inimigosOnda = replicate 5 inimJogo3Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 5 inimJogo3Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = replicate 2 inimBlinJogo3Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 5 inimJogo3Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 3.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimBlinJogo3Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 10 inimJogo3Fase2Portal1,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 3.0
                },
                Onda {
                    inimigosOnda = replicate 5 (inimBlinJogo3Portal1 {vidaInimigo = 500}),
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 10 (inimBlinJogo3Portal1 {vidaInimigo = 500}),
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 10.0
                },
                Onda {
                    inimigosOnda = replicate 3 (inimBlinJogo3Portal1 {vidaInimigo = 500}) ++ replicate 1 inimBossJogo3Portal1 ++ replicate 3 (inimBlinJogo3Portal1 {vidaInimigo = 500}),
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 10.0
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
                    inimigosOnda = replicate 5 inimJogo3Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 5 inimJogo3Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 8.0
                },
                Onda {
                    inimigosOnda = replicate 2 inimBlinJogo3Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 5 inimJogo3Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 3.0
                },
                Onda {
                    inimigosOnda = replicate 3 inimBlinJogo3Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 10 inimJogo3Fase2Portal2,
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 3.0
                },
                Onda {
                    inimigosOnda = replicate 5 (inimBlinJogo3Portal2 {vidaInimigo = 500}),
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 15.0
                },
                Onda {
                    inimigosOnda = replicate 10 (inimBlinJogo3Portal2 {vidaInimigo = 500}),
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 10.0
                },
                Onda {
                    inimigosOnda = replicate 3 (inimBlinJogo3Portal2 {vidaInimigo = 500}) ++ replicate 1 inimBossJogo3Portal2 ++ replicate 3 (inimBlinJogo3Portal2 {vidaInimigo = 500}),
                    cicloOnda = 1.0,
                    tempoOnda = 1.0,
                    entradaOnda = 10.0
                }
            ]}
    ],
    torresJogo = [],
    mapaJogo = mapa03,
    inimigosJogo = [],
    lojaJogo = [
        (50, Torre {
            posicaoTorre = (0, 0),
            danoTorre = 35,
            alcanceTorre = 1.5,
            rajadaTorre = 4,
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

inimBlinJogo1Portal1 :: Inimigo
inimBlinJogo1Portal1 = Inimigo {posicaoInimigo = (0.5, 2.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 350,
                          velocidadeInimigo = 0.65,
                          ataqueInimigo = 30,
                          butimInimigo = 50,
                          projeteisInimigo = [],
                          tipoInimigo = Blindado
                         }

-- | Inimigos para o jogo2:

inimJogo2Fase1Portal1 :: Inimigo
inimJogo2Fase1Portal1 = inimJogo1Fase1Portal1 {posicaoInimigo = (1.5, 1.5)}

inimJogo2Fase2Portal1 :: Inimigo
inimJogo2Fase2Portal1 = inimJogo1Fase2Portal1 {posicaoInimigo = (1.5, 1.5)}

inimJogo2Fase1Portal2 :: Inimigo
inimJogo2Fase1Portal2 = inimJogo2Fase1Portal1 {posicaoInimigo = (9.5, 9.5), direcaoInimigo = Oeste}

inimJogo2Fase2Portal2 :: Inimigo
inimJogo2Fase2Portal2 = inimJogo2Fase2Portal1 {posicaoInimigo = (9.5, 9.5), direcaoInimigo = Oeste}

inimBlinJogo2Portal1 :: Inimigo
inimBlinJogo2Portal1 = inimBlinJogo1Portal1 {posicaoInimigo = (1.5, 1.5)}

inimBlinJogo2Portal2 :: Inimigo
inimBlinJogo2Portal2 = inimBlinJogo1Portal1 {posicaoInimigo = (9.5, 9.5), direcaoInimigo = Oeste}

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

inimBlinJogo3Portal1 :: Inimigo
inimBlinJogo3Portal1 = Inimigo {posicaoInimigo = (0.5, 4.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 350,
                          velocidadeInimigo = 0.65,
                          ataqueInimigo = 30,
                          butimInimigo = 50,
                          projeteisInimigo = [],
                          tipoInimigo = Blindado
                          }

inimBossJogo3Portal1 :: Inimigo
inimBossJogo3Portal1 = Inimigo {posicaoInimigo = (0.5, 4.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 1250,
                          velocidadeInimigo = 0.5,
                          ataqueInimigo = 50,
                          butimInimigo = 100,
                          projeteisInimigo = [],
                          tipoInimigo = BossBlindado
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
                          tipoInimigo = Normal
                          }

inimBlinJogo3Portal2 :: Inimigo
inimBlinJogo3Portal2 = Inimigo {posicaoInimigo = (0.5, 6.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 350,
                          velocidadeInimigo = 0.65,
                          ataqueInimigo = 30,
                          butimInimigo = 50,
                          projeteisInimigo = [],
                          tipoInimigo = Blindado
                          }

inimBossJogo3Portal2 :: Inimigo
inimBossJogo3Portal2 = Inimigo {posicaoInimigo = (0.5, 6.5),
                          direcaoInimigo = Este,
                          vidaInimigo = 1250,
                          velocidadeInimigo = 0.5,
                          ataqueInimigo = 50,
                          butimInimigo = 100,
                          projeteisInimigo = [],
                          tipoInimigo = BossBlindado
                          }