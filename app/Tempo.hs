module Tempo where

import ImmutableTowers
import LI12425
import Tarefa2
import Tarefa3

{-| Função que atualiza o jogo conforme o tempo passa. 

Recebe o tempo (t) que passou desde a última atualização e o jogo atual e retorna o jogo atualizado.

Como implementamos um menu no jogo, esta função so atualiza o jogo se o jogador se encontrar com o jogo em andamento. 
-}

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo _ it@(ImmutableTowers {menu = MenuInicial}) = it
reageTempo t it@(ImmutableTowers {jogoAtual = jogoAtual, menu = ModoJogo estado}) = 
    case estado of
        EmAndamento -> if ganhouJogo jogoAtual
                       then (it {menu = ModoJogo GanhouJogo})
                       else if perdeuJogo jogoAtual
                       then (it {menu = ModoJogo PerdeuJogo})
                       else (it {jogoAtual = atualizaJogo t jogoAtual})
        _ -> it