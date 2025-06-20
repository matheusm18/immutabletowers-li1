module Exemplos where
import LI12425

-- Mapas:

mapa01 :: Mapa
mapa01 =
    [ [t,t,a,a,r,r,r,r],
      [a,t,a,a,r,t,t,t],
      [a,t,r,r,r,t,r,a],
      [t,t,r,a,r,t,r,a],
      [a,t,a,a,r,t,t,a],
      [a,t,r,r,r,r,t,a],
      [a,t,t,t,t,t,t,a],
      [a,a,t,a,a,a,a,a]
    ]
  where
    t = Terra
    r = Relva
    a = Agua

mapa02 :: Mapa
mapa02 = 
  [ [a,a,a,a,a,a,a,a,a,a],
    [a,a,a,a,r,r,r,r,r,r],
    [t,t,t,a,r,t,t,t,t,r],
    [a,r,t,a,r,t,a,a,t,r],
    [a,r,t,a,r,t,a,a,t,r],
    [a,r,t,a,r,t,a,a,t,r],
    [a,r,t,a,r,t,a,a,t,r],
    [a,r,t,t,t,t,a,a,t,t],
    [a,r,r,t,r,r,a,a,r,r],
    [a,a,a,a,a,a,a,a,a,a]
  ]
  where
    t = Terra
    r = Relva
    a = Agua

-- Torres:

torre01 :: Torre
torre01 = Torre { posicaoTorre = (2.5, 5.5),
                  danoTorre = 25,
                  alcanceTorre = 5,
                  rajadaTorre = 3,
                  cicloTorre = 5,
                  tempoTorre = 3,
                  projetilTorre = Projetil Fogo (Finita 5),
                  nivelTorre = 1
                }

torre02 :: Torre
torre02 = Torre { posicaoTorre = (1.5, 4.5),
                  danoTorre = 35,
                  alcanceTorre = 4,
                  rajadaTorre = 4,
                  cicloTorre = 4,
                  tempoTorre = 2,
                  projetilTorre = Projetil Gelo (Finita 10),
                  nivelTorre = 1
                }

torre03 :: Torre
torre03 = Torre { posicaoTorre = (4.5, 4.5),
                  danoTorre = 40,
                  alcanceTorre = 5,
                  rajadaTorre = 4,
                  cicloTorre = 5,
                  tempoTorre = 0,
                  projetilTorre = Projetil Resina (Finita 10),
                  nivelTorre = 1
                }      

torreInvalida :: Torre
torreInvalida = Torre { posicaoTorre = (4.5, 8.5),
                        danoTorre = 25,
                        alcanceTorre = -3,
                        rajadaTorre = -2,
                        cicloTorre = 5,
                        tempoTorre = 3,
                        projetilTorre = Projetil Fogo (Finita 5),
                        nivelTorre = 1
                      }

-- Bases:

base01 :: Base
base01 = Base { vidaBase = 250,
                posicaoBase = (7.5, 1.5),
                creditosBase = 150
              }

base02 :: Base
base02 = Base { vidaBase = 200,
                posicaoBase = (9.5, 7.5),
                creditosBase = 250
              }

baseSemVida :: Base
baseSemVida = Base { vidaBase = 0,
                     posicaoBase = (7.5, 1.5),
                     creditosBase = 150
                   }

-- Inimigos:

inimigo01 :: Inimigo
inimigo01 = Inimigo { posicaoInimigo = (0.5, 0.5),
                      direcaoInimigo = Este,
                      vidaInimigo = 175,
                      velocidadeInimigo = 2,
                      ataqueInimigo = 20,
                      butimInimigo = 25,
                      projeteisInimigo = [],
                      tipoInimigo = Normal
                    }

inimigo02 :: Inimigo
inimigo02 = Inimigo { posicaoInimigo = (0.5, 2.5),
                      direcaoInimigo = Este,
                      vidaInimigo = 100,
                      velocidadeInimigo = 1,
                      ataqueInimigo = 5,
                      butimInimigo = 10,
                      projeteisInimigo = [],
                      tipoInimigo = Normal
                    }

inimigo03 :: Inimigo
inimigo03 = Inimigo { posicaoInimigo = (0.5, 2.5),
                      direcaoInimigo = Este,
                      vidaInimigo = 150,
                      velocidadeInimigo = 2.5,
                      ataqueInimigo = 10,
                      butimInimigo = 15,
                      projeteisInimigo = [],
                      tipoInimigo = Normal
                    }

inimigo04 :: Inimigo
inimigo04 = Inimigo { posicaoInimigo = (3.5, 8.5),
                      direcaoInimigo = Norte,
                      vidaInimigo = 200,
                      velocidadeInimigo = 3,
                      ataqueInimigo = 15,
                      butimInimigo = 20,
                      projeteisInimigo = [],
                      tipoInimigo = Normal
                    }

inimigoEmJogo01 :: Inimigo
inimigoEmJogo01 = Inimigo { posicaoInimigo = (1.5, 5.5),
                            direcaoInimigo = Sul,
                            vidaInimigo = 100,
                            velocidadeInimigo = 2,
                            ataqueInimigo = 20,
                            butimInimigo = 25,
                            projeteisInimigo = [],
                            tipoInimigo = Normal
                          }

inimigoEmJogo02 :: Inimigo
inimigoEmJogo02 = Inimigo { posicaoInimigo = (8.5, 6.5),
                            direcaoInimigo = Sul,
                            vidaInimigo = 150,
                            velocidadeInimigo = 1,
                            ataqueInimigo = 20,
                            butimInimigo = 25,
                            projeteisInimigo = [],
                            tipoInimigo = Normal
                          }
-- Ondas:

onda01 :: Onda
onda01 = Onda { inimigosOnda = [inimigo01, inimigo01],
                cicloOnda = 5,
                tempoOnda = 3,
                entradaOnda = 0
              }

onda02 :: Onda
onda02 = Onda { inimigosOnda = [inimigo02, inimigo03],
                cicloOnda = 5,
                tempoOnda = 3,
                entradaOnda = 3
              }

onda03 :: Onda
onda03 = Onda { inimigosOnda = [inimigo04, inimigo04],
                cicloOnda = 10,
                tempoOnda = 0,
                entradaOnda = 0
              }

-- Portais:

portal01 :: Portal
portal01 = Portal { posicaoPortal = (0.5, 0.5),
                    ondasPortal = [onda01]
                  }

portal02 :: Portal
portal02 = Portal { posicaoPortal = (0.5, 2.5),
                    ondasPortal = [onda02]
                  }

portal03 :: Portal
portal03 = Portal { posicaoPortal = (3.5, 8.5),
                    ondasPortal = [onda03]
                  }

portalSemOndas :: Portal
portalSemOndas = Portal { posicaoPortal = (0.5, 0.5),
                         ondasPortal = []
                       }
-- Jogos:

jogo01 :: Jogo
jogo01 = Jogo { baseJogo = base01,
                portaisJogo = [portal01],
                torresJogo = [torre01],
                mapaJogo = mapa01,
                inimigosJogo = [inimigoEmJogo01],
                lojaJogo = [(50, torre01), (50, torre02), (50, torre03)],
                precoUpgrades = [(50, 1, Gelo),(50,1,Fogo),(50,1,Resina),(75,2,Gelo),(75,2,Fogo),(75,2,Resina),(100,3,Gelo),(100,3,Fogo),(100,3,Resina)]
              }

jogo02 :: Jogo
jogo02 = Jogo { baseJogo = base02,
                portaisJogo = [portal02, portal03],
                torresJogo = [torre02,torre03],
                mapaJogo = mapa02,
                inimigosJogo = [inimigoEmJogo02],
                lojaJogo = [(50, torre01), (50, torre02), (50, torre03)],
                precoUpgrades = [(50, 1, Gelo),(50,1,Fogo),(50,1,Resina),(75,2,Gelo),(75,2,Fogo),(75,2,Resina),(100,3,Gelo),(100,3,Fogo),(100,3,Resina)]
              }

jogoInvalido :: Jogo
jogoInvalido = Jogo { baseJogo = base01,
                      portaisJogo = [],
                      torresJogo = [torre01],
                      mapaJogo = mapa02,
                      inimigosJogo = [],
                      lojaJogo = [(50, torre01), (50, torre02), (50, torre03)],
                      precoUpgrades = [(50, 1, Gelo),(50,1,Fogo),(50,1,Resina),(75,2,Gelo),(75,2,Fogo),(75,2,Resina),(100,3,Gelo),(100,3,Fogo),(100,3,Resina)]
                    }

jogoGanho :: Jogo
jogoGanho = Jogo { baseJogo = base01,
                   portaisJogo = [portalSemOndas],
                   torresJogo = [torre01],
                   mapaJogo = mapa01,
                   inimigosJogo = [],
                   lojaJogo = [(50, torre01), (50, torre02), (50, torre03)],
                   precoUpgrades = [(50, 1, Gelo),(50,1,Fogo),(50,1,Resina),(75,2,Gelo),(75,2,Fogo),(75,2,Resina),(100,3,Gelo),(100,3,Fogo),(100,3,Resina)]
                 }

jogoPerdido :: Jogo
jogoPerdido = Jogo { baseJogo = baseSemVida,
                     portaisJogo = [portal01],
                     torresJogo = [torre01],
                     mapaJogo = mapa01,
                     inimigosJogo = [inimigoEmJogo01],
                     lojaJogo = [(50, torre01), (50, torre02), (50, torre03)],
                     precoUpgrades = [(50, 1, Gelo),(50,1,Fogo),(50,1,Resina),(75,2,Gelo),(75,2,Fogo),(75,2,Resina),(100,3,Gelo),(100,3,Fogo),(100,3,Resina)]
                   }

-- | Jogo esperado após atualização do jogo01 com tempo = 0.5
jogoAposAtualizaJogo01 :: Jogo
jogoAposAtualizaJogo01 = Jogo { baseJogo = base01,
                               portaisJogo = [portal01 {ondasPortal = [onda01 {tempoOnda = 2.5}]}],
                               torresJogo = [torre01 {tempoTorre = 2.5}],
                               mapaJogo = mapa01,
                               inimigosJogo = [inimigoEmJogo01 {posicaoInimigo = (1.5, 6.5), direcaoInimigo = Sul, vidaInimigo = 100.0, projeteisInimigo = []}],
                               lojaJogo = [(50, torre01), (50, torre02), (50, torre03)],
                               precoUpgrades = [(50, 1, Gelo),(50,1,Fogo),(50,1,Resina),(75,2,Gelo),(75,2,Fogo),(75,2,Resina),(100,3,Gelo),(100,3,Fogo),(100,3,Resina)]
                             }