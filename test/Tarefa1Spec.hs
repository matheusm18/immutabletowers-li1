module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import Exemplos

-- | Testes para a função tipoTerreno:
testTipoTerreno :: Test
testTipoTerreno = TestList
  [ "tipoTerreno Terra" ~: Just Terra ~=? tipoTerreno (0.5,0.7) mapa01,
    "tipoTerreno Relva" ~: Just Relva ~=? tipoTerreno (5.5,0.5) mapa01,
    "tipoTerreno Agua" ~: Just Agua ~=? tipoTerreno (2.5,0.5) mapa01,
    "tipoTerreno (Posicao fora do mapa)" ~: Nothing ~=? tipoTerreno (-2,-3) mapa02
  ]

-- | Testes para a função validaPosicaoTerra:
testValidaPosicaoTerra :: Test
testValidaPosicaoTerra = TestList
  [ "validaPosicaoTerra True" ~: True ~=? validaPosicaoTerra (1.5,1.5) mapa01,
    "validaPosicaoTerra False" ~: False ~=? validaPosicaoTerra (0.5,0.5) mapa02
  ]

-- | Testes para a função validaPosicaoRelva:
testValidaPosicaoRelva :: Test
testValidaPosicaoRelva = TestList
  [ "validaPosicaoRelva True" ~: True ~=? validaPosicaoRelva (4.8,0.4) mapa01,
    "validaPosicaoRelva False" ~: False ~=? validaPosicaoRelva (1.5,0.5) mapa01
  ]

-- | Testes para a função validaPortais:
testValidaPortais :: Test
testValidaPortais = TestList
  [ "validaPortais True" ~: True ~=? validaPortais [portal01] mapa01 base01 [torre01],
    "validaPortais False" ~: False ~=? validaPortais [] mapa01 base01 [torre01]
  ]

-- | Testes para a função validaInimigosPortal:
testValidaInimigosPortal :: Test
testValidaInimigosPortal = TestList
  [ "validaInimigosPortal True" ~: True ~=? validaInimigosPortal (0.5,0.5) [onda01],
    "validaInimigosPortal False" ~: False ~=? validaInimigosPortal (2.5,3.5) [onda02]
  ]

-- | Testes para a função validaInimigosEmJogo:
testValidaInimigosEmJogo :: Test
testValidaInimigosEmJogo = TestList
  [ "validaInimigosEmJogo True" ~: True ~=? validaInimigosEmJogo [inimigo02,inimigo03, inimigo04] [torre02,torre03] mapa02
    "validaInimigosEmJogo False" ~: False ~=? validaInimigosEmJogo [inimigo01, inimigo02] [torre02,torre03] mapa02
  ]

-- | Testes para a função validaTorres:
testValidaTorres :: Test
testValidaTorres = TestList
  [ "validaTorres True" ~: True ~=? validaTorres [torre02,torre03] mapa02
    "validaTorres False" ~: False ~=? validaTorres [torre02,torreInvalida] mapa02
  ]

-- | Testes para a função validaBase:
testValidaBase :: Test
testValidaBase = TestList
  [ "validaBase True" ~: True ~=? validaBase base01 mapa01 [torre01] [portal01],
    "validaBase False" ~: False ~=? validaBase base01 mapa02 [torre02,torre03] [portal02,portal03]
  ]

-- | Testes para a função validaJogo:
testValidaJogo :: Test
testValidaJogo = TestList
  [ "validaJogo True" ~: True ~=? validaJogo jogo01
    "validaJogo False" ~: False ~=? validaJogo jogoInvalido
  ]

testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ testTipoTerreno,
        testValidaPosicaoTerra,
        testValidaPosicaoRelva,
        testValidaPortais,
        testValidaInimigosPortal,
        testValidaInimigosEmJogo,
        testValidaTorres,
        testValidaBase,
        testValidaJogo
      ]