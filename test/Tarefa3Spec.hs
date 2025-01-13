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
  
-- | Testes para a função getPosicoesValidas

testeGetPosicoesValidas :: Test
testeGetPosicoesValidas = TestList
  [
    "getPosicoesValidas (1.5,1.5) Este [[Terra, Terra, Agua], [Terra,Terra,Agua], [Relva,Terra,Agua]]" ~: [((1.5,0.999),Norte),((1.5,2.001),Sul)] ~=? getPosicoesValidas (1.5,1.5) Este [[Terra, Terra, Agua], [Terra,Terra,Agua], [Relva,Terra,Agua]],
    "getPosicoesValidas (2.5,1.5) Este [[Terra, Agua, Agua,Agua],[Terra,Terra,Terra,Terra],[Relva,Relva,Relva,Terra],[Relva,Terra,Terra,Terra]]" ~: [((3.001,1.5),Este)] ~=? getPosicoesValidas (2.5,1.5) Este [[Terra, Agua, Agua,Agua],[Terra,Terra,Terra,Terra],[Relva,Relva,Relva,Terra],[Relva,Terra,Terra,Terra]]
  ]

testeEscolheDirecao :: Test
testeEscolheDirecao = TestList
  [
    "escolheDirecao [((1.001,0.5),Este)] [(1,0),(1,1)]" ~: Just Este ~=? escolheDirecao [((1.001,0.5),Este)] [(1,0),(1,1)],
    "escolheDirecao [((2.001,0.5),Norte),((2.5,1.001),Sul)] [(2,1),(2,2)]" ~: Just Sul ~=? escolheDirecao [((2.001,0.5),Norte),((2.5,1.001),Sul)] [(2,1),(2,2)]
  ]

testePertenceCaminho :: Test
testePertenceCaminho = TestList
  [
    "pertenceCaminho (1.5,1.5) [(1,1),(1,2),(1,3),(2,3),(3,3)]" ~: True ~=? pertenceCaminho (1.5,1.5) [(1,1),(1,2),(1,3),(2,3),(3,3)],
    "pertenceCaminho (0.5,0.5) [(1,1),(1,2),(1,3),(2,3),(3,3)]" ~: False ~=? pertenceCaminho (0.5,0.5) [(1,1),(1,2),(1,3),(2,3),(3,3)]
  ]

testeGetPosCentroQuadrado :: Test
testeGetPosCentroQuadrado = TestList
  [
    "getPosCentroQuadrado (9.0,4.75)" ~: (9.5,4.5) ~=? getPosCentroQuadrado (9.0,4.75),
    "getPosCentroQuadrado (0.0,1.0)" ~: (0.5,1.5) ~=? getPosCentroQuadrado (0.0,1.0)
  ]

testeMoveInimigo :: Test
testeMoveInimigo = TestList
  [
    "moveInimigo 1 inimigo01 (7.5, 1.5) mapa01" ~: (Este,(2.5,0.5)) ~=? moveInimigo 1 inimigo01 (7.5, 1.5) mapa01,
    "moveInimigo 1 inimigo02 (9.5,7.5) mapa02" ~: (Este,(1.5,2.5)) ~=? moveInimigo 1 inimigo02 (9.5,7.5) mapa02
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

-- | Testes para a função getDanoNaBase
testeGetDanoNaBase :: Test
testeGetDanoNaBase = TestList
  [
    "getDanoNaBase [inimigo01 {posicaoInimigo = (7.5,1.5)}] base01" ~: 20 ~=? getDanoNaBase [inimigo01 {posicaoInimigo = (7.5,1.5)}] base01,
    "getDanoNaBase [inimigo02] base02" ~: 0 ~=? getDanoNaBase [inimigo02] base02
  ]

-- | Testes para a função getButim
testeGetButim :: Test
testeGetButim = TestList
  [
    "getButim [inimigo01 {vidaInimigo = 0}]" ~: 25 ~=? getButim [inimigo01 {vidaInimigo = 0}],
    "getButim [inimigo02]" ~: 0 ~=? getButim [inimigo02]
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
        testeGetPosicoesValidas,
        testeEscolheDirecao,
        testePertenceCaminho,
        testeGetPosCentroQuadrado,
        testeMoveInimigo,
        testeAtualizaDurProjetil,
        testeDuracaoExpirou,
        testeAtualizaInimigos,
        testeGetDanoNaBase,
        testeGetButim,
        testeAtualizaBase,
        testeAtualizaJogo
      ]
