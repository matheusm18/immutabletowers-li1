module ImmutableTowers where

import Graphics.Gloss
import LI12425

data OpcaoMenu = Jogar | Sair deriving Eq

data EstadoJogo = EmAndamento | PerdeuJogo | GanhouJogo deriving Eq

data Menu = MenuInicial OpcaoMenu | ModoJogo EstadoJogo deriving Eq

data ImmutableTowers = ImmutableTowers { jogoInicial :: Jogo,
                                         jogoAtual :: Jogo, 
                                         menu :: Menu, 
                                         imagens :: [(String,Picture)],
                                         torreSelecionada :: Maybe Torre}