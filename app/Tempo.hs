module Tempo where

import ImmutableTowers
import LI12425
import Tarefa2
import Tarefa3


reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo t it@(ImmutableTowers _ _ (MenuInicial _)) = it
reageTempo t it@(ImmutableTowers {jogoAtual = jogoAtual, menu = ModoJogo estado}) = 
    case estado of
        EmAndamento -> if ganhouJogo jogoAtual
                       then (it {menu = ModoJogo GanhouJogo})
                       else if perdeuJogo jogoAtual
                       then (it {menu = ModoJogo PerdeuJogo})
                       else (it {jogoAtual = atualizaJogo t jogoAtual})
        _ -> it