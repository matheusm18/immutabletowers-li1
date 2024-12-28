module Tempo where

import ImmutableTowers
import LI12425
import Tarefa2
import Tarefa3

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t (ImmutableTowers jogo) = ImmutableTowers (atualizaJogo t jogo)
