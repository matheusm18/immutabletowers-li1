module Tarefa2Spec (testesTarefa2) where

import Test.HUnit
import Tarefa2
import Exemplos
import LI12425

-- | Testes para a função dist
testeDist :: Test
testeDist = TestList
  [ "dist (0,0) (0,2)" ~: 2.0 ~=? dist (0,0) (0,2),
    "dist (0,0) (3,4)" ~: 5.0 ~=? dist (0,0) (3,4)
  ]

-- | Testes para a função inimigosNoAlcance
testeInimigosNoAlcance :: Test
testeInimigosNoAlcance = TestList
  [ "inimigosNoAlcance torre01 [inimigoEmJogo01]" ~: [inimigoEmJogo01] ~=? inimigosNoAlcance torre01 [inimigoEmJogo01],
    "inimigosNoAlcance torre02 [inimigoEmJogo02]" ~: [] ~=? inimigosNoAlcance torre02 [inimigoEmJogo02]
  ]

-- | Testes para a função verificaGelo
testeVerificaGelo :: Test
testeVerificaGelo = TestList
  [ "verificaGelo [Projetil Gelo (Finita 5)]" ~: True ~=? verificaGelo [Projetil Gelo (Finita 5)],
    "verificaGelo [Projetil Fogo (Finita 5)]" ~: False ~=? verificaGelo [Projetil Fogo (Finita 5)]
  ]

-- | Testes para a função verificaFogo
testeVerificaFogo :: Test
testeVerificaFogo = TestList
  [ "verificaFogo [Projetil Fogo (Finita 10)]" ~: True ~=? verificaFogo [Projetil Fogo (Finita 10)],
    "verificaFogo [Projetil Gelo (Finita 3)]" ~: False ~=? verificaFogo [Projetil Gelo (Finita 3)]
  ]

-- | Testes para a função verificaResina
testeVerificaResina :: Test
testeVerificaResina = TestList
  [ "verificaResina [Projetil Resina (Finita 7)]" ~: True ~=? verificaResina [Projetil Resina (Finita 7)],
    "verificaResina [Projetil Fogo (Finita 5)]" ~: False ~=? verificaResina [Projetil Fogo (Finita 5)]
  ]

-- | Testes para a função dobraDuracao
testeDobraDuracao :: Test
testeDobraDuracao = TestList
  [ "dobraDuracao (Projetil Fogo (Finita 5))" ~: Finita 10 ~=? dobraDuracao (Projetil Fogo (Finita 5)),
    "dobraDuracao (Projetil Gelo Infinita)" ~: Infinita ~=? dobraDuracao (Projetil Gelo Infinita)
  ]

-- | Testes para a função somaDuracao
testeSomaDuracao :: Test
testeSomaDuracao = TestList
  [ "somaDuracao (Projetil Fogo (Finita 5)) (Projetil Fogo (Finita 10))" ~: Finita 15 ~=? somaDuracao (Projetil Fogo (Finita 5)) (Projetil Fogo (Finita 10)),
    "somaDuracao (Projetil Gelo Infinita) (Projetil Gelo (Finita 10))" ~: Infinita ~=? somaDuracao (Projetil Gelo Infinita) (Projetil Gelo (Finita 10))
  ]

-- | Testes para a função atualizaProjeteis
testeAtualizaProjeteis :: Test
testeAtualizaProjeteis = TestList
  [ "atualizaProjeteis (Projetil Fogo (Finita 5)) [Projetil Gelo (Finita 10)]" ~: [] ~=? atualizaProjeteis (Projetil Fogo (Finita 5)) [Projetil Gelo (Finita 10)],
    "atualizaProjeteis (Projetil Fogo (Finita 5)) [Projetil Resina (Finita 10)]" ~: [Projetil Fogo (Finita 10)] ~=? atualizaProjeteis (Projetil Fogo (Finita 5)) [Projetil Resina (Finita 10)],
    "atualizaProjeteis (Projetil Resina (Finita 5)) [Projetil Gelo (Finita 10)]" ~: [Projetil Resina (Finita 5),Projetil Gelo (Finita 10)] ~=? atualizaProjeteis (Projetil Resina (Finita 5)) [Projetil Gelo (Finita 10)]
  ]

-- | Testes para a função atingeInimigo
testeAtingeInimigo :: Test
testeAtingeInimigo = TestList
  [ "atingeInimigo torre01 inimigoEmJogo01" ~: inimigoEmJogo01 {vidaInimigo = 75, projeteisInimigo = [Projetil Fogo (Finita 5)]} ~=? atingeInimigo torre01 inimigoEmJogo01,
    "atingeInimigo torre03 inimigoEmJogo02" ~: inimigoEmJogo02 {vidaInimigo = 110, projeteisInimigo = [Projetil Resina (Finita 10)]} ~=? atingeInimigo torre03 inimigoEmJogo02
  ]

-- | Testes para a função ativaInimigo
testeAtivaInimigo :: Test
testeAtivaInimigo = TestList
  [ "ativaInimigo portal01 []" ~: (portal01 {ondasPortal = [onda01 {inimigosOnda = [inimigo01], tempoOnda = 5}]}, [inimigo01]) ~=? ativaInimigo portal01 [],
    "ativaInimigo portal02 [inimigoEmJogo02]" ~: (portal02 {ondasPortal = [onda02 {inimigosOnda = [inimigo03], tempoOnda = 5}]}, [inimigo02, inimigoEmJogo02]) ~=? ativaInimigo portal02 [inimigoEmJogo02]
  ]

-- | Testes para a função terminouJogo
testeTerminouJogo :: Test
testeTerminouJogo = TestList
  [ "terminouJogo jogoGanho" ~: True ~=? terminouJogo jogoGanho,
    "terminouJogo jogo01" ~: False ~=? terminouJogo jogo01
  ]

-- | Testes para a função ganhouJogo
testeGanhouJogo :: Test
testeGanhouJogo = TestList
  [ "ganhouJogo jogoGanho" ~: True ~=? ganhouJogo jogoGanho,
    "ganhouJogo jogo01" ~: False ~=? ganhouJogo jogo01
  ]

-- | Testes para a função perdeuJogo
testePerdeuJogo :: Test
testePerdeuJogo = TestList
  [ "perdeuJogo jogoPerdido" ~: True ~=? perdeuJogo jogoPerdido,
    "perdeuJogo jogo02" ~: False ~=? perdeuJogo jogo02
  ]

testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ testeDist,
        testeInimigosNoAlcance,
        testeVerificaGelo,
        testeVerificaFogo,
        testeVerificaResina,
        testeDobraDuracao,
        testeSomaDuracao,
        testeAtualizaProjeteis,
        testeAtingeInimigo,
        testeAtivaInimigo,
        testeTerminouJogo,
        testeGanhouJogo,
        testePerdeuJogo
      ]
