module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import Tarefa3
import Exemplos
import LI12425

-- | Testes para a função atualizaPortal
testeAtualizaPortal :: Test
testeAtualizaPortal = TestList
  [ "atualizaPortal 1 portal01 []" ~: (portal01 {ondasPortal = [onda01 {tempoOnda = 2}]}, []) ~=? atualizaPortal 1 portal01 [],
    "atualizaPortal 1 portal03 []" ~: (portal03 {ondasPortal = [onda03 {tempoOnda = 10, inimigosOnda = [inimigo04]}]}, [inimigo04]) ~=? atualizaPortal 1 portal03 []
  ]

-- | Testes para a função atualizaPortais
testeAtualizaPortais :: Test
testeAtualizaPortais = TestList
  [ "atualizaPortais 1 [portal02, portal03] []" ~: ([portal02 {ondasPortal = [onda02 {entradaOnda = 2}]}, portal03 {ondasPortal = [onda03 {tempoOnda = 10, inimigosOnda = [inimigo04]}]}], [inimigo04]) ~=? atualizaPortais 1 [portal02, portal03] [],
    "atualizaPortais 3 [portal02, portal03] []" ~: ([portal02 {ondasPortal = [onda02 {entradaOnda = 0}]}, portal03 {ondasPortal = [onda03 {tempoOnda = 10, inimigosOnda = [inimigo04]}]}], [inimigo04]) ~=? atualizaPortais 3 [portal02, portal03] []
  ]

-- | Testes para a função atacaInimigos
testeAtacaInimigos :: Test
testeAtacaInimigos = TestList
  [ "atacaInimigos torre01 [inimigoEmJogo01, inimigo01]" ~: (torre01 {tempoTorre = 5}, [inimigoEmJogo01 {vidaInimigo = 75, projeteisInimigo = [Projetil Fogo (Finita 5)]}, inimigo01]) ~=? atacaInimigos torre01 [inimigoEmJogo01, inimigo01],
    "atacaInimigos torre02 [inimigoEmJogo02]" ~: (torre02,[inimigoEmJogo02]) ~=? atacaInimigos torre02 [inimigoEmJogo02]
  ]

-- | Testes para a função atualizaTorres
testeAtualizaTorres :: Test
testeAtualizaTorres = TestList
  [ "atualizaTorres 1 [torre01] [inimigoEmJogo01]" ~: ([torre01 {tempoTorre = 2}], [inimigoEmJogo01]) ~=? atualizaTorres 1 [torre01] [inimigoEmJogo01],
    "atualizaTorres 2 [torre02, torre03] [inimigoEmJogo02]" ~: ([torre03 {tempoTorre = 5}, torre02 {tempoTorre = 0}], [inimigoEmJogo02 {vidaInimigo = 110, projeteisInimigo = [Projetil Resina (Finita 10)]}]) ~=? atualizaTorres 2 [torre02, torre03] [inimigoEmJogo02]
  ]
  
-- | Testes para a função atualizaDurProjetil
testeAtualizaDurProjetil :: Test
testeAtualizaDurProjetil = TestList
  [ "atualizaDurProjetil 3 (Projetil Fogo (Finita 5))" ~: Projetil Fogo (Finita 2) ~=? atualizaDurProjetil 3 (Projetil Fogo (Finita 5)),
    "atualizaDurProjetil 1 (Projetil Gelo Infinita)" ~: Projetil Gelo Infinita ~=? atualizaDurProjetil 1 (Projetil Gelo Infinita)
  ]

-- | Testes para a função duracaoExpirou
testeDuracaoExpirou :: Test
testeDuracaoExpirou = TestList
  [ "duracaoExpirou (Projetil Fogo (Finita 0))" ~: True ~=? duracaoExpirou (Projetil Fogo (Finita 0)),
    "duracaoExpirou (Projetil Gelo (Finita 5))" ~: False ~=? duracaoExpirou (Projetil Gelo (Finita 5))
  ]

-- | Testes para a função atualizaInimigos
testeAtualizaInimigos :: Test
testeAtualizaInimigos = TestList
  [ "atualizaInimigos 0.5 mapa01 base01 [inimigoEmJogo01]" ~: ([inimigoEmJogo01 {posicaoInimigo = (1.5, 6.5), direcaoInimigo = Sul, vidaInimigo = 100.0, projeteisInimigo = []}], 0, 0) ~=? atualizaInimigos 0.5 mapa01 base01 [inimigoEmJogo01]
  ]

-- | Testes para a função atualizaBase
testeAtualizaBase :: Test
testeAtualizaBase = TestList
  [ "atualizaBase 1 base01 20 50" ~: base01 {vidaBase = 230, creditosBase = 200} ~=? atualizaBase 1 base01 20 50,
    "atualizaBase 1 base01 0 0" ~: base01 ~=? atualizaBase 1 base01 0 0
  ]

-- | Testes para a função atualizaJogo
testeAtualizaJogo :: Test
testeAtualizaJogo = TestList
  [ "atualizaJogo 0.5 jogo01" ~: jogoAposAtualizaJogo01 ~=? atualizaJogo 0.5 jogo01
  ]

testesTarefa3 :: Test
testesTarefa3 =
  TestLabel "Testes Tarefa 3" $
    test
      [ testeAtualizaPortal,
        testeAtualizaPortais,
        testeAtacaInimigos,
        testeAtualizaTorres,
        testeAtualizaDurProjetil,
        testeDuracaoExpirou,
        testeAtualizaInimigos,
        testeAtualizaBase,
        testeAtualizaJogo
      ]
