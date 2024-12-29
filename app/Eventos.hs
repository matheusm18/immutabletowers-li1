module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers

reageEventosMenu :: Event -> ImmutableTowers -> ImmutableTowers
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {menu = MenuInicial Jogar}) = it {menu = ModoJogo EmAndamento}
reageEventosMenu (EventKey (SpecialKey KeyDown) Down _ _) it@(ImmutableTowers {menu = MenuInicial Jogar}) = it {menu = MenuInicial Sair}
reageEventosMenu (EventKey (SpecialKey KeyUp) Down _ _) it@(ImmutableTowers {menu = MenuInicial Sair}) = it {menu = MenuInicial Jogar}
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {jogoInicial = jogoInicio, menu = ModoJogo PerdeuJogo}) = it {jogoAtual = jogoInicio, menu = MenuInicial Jogar}
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {jogoInicial = jogoInicio, menu = ModoJogo GanhouJogo}) = it {jogoAtual = jogoInicio, menu = MenuInicial Jogar}
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {menu = MenuInicial Sair})  = undefined -- ver como faz pra fechar o jogo
reageEventosMenu _ it = it

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos _ it@(ImmutableTowers {menu = ModoJogo EmAndamento}) = it -- ainda falta depois adicionarmos como comprar as coisas na loja
reageEventos k it@(ImmutableTowers {menu = MenuInicial m}) = reageEventosMenu k (it {menu = MenuInicial m})
reageEventos k it@(ImmutableTowers {menu = ModoJogo m}) = reageEventosMenu k (it {menu = ModoJogo m})
reageEventos _ it = it
