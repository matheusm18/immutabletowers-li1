module Main (main) where

import Test.HUnit

import Tarefa1Spec
import Tarefa2Spec
import Tarefa3Spec

-- | Função que corre todos os testes.
main :: IO ()
main = runTestTTAndExit $ test [testesTarefa1, testesTarefa2, testesTarefa3]
