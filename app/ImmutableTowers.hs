module ImmutableTowers where

import Graphics.Gloss
import LI12425

data EstadoJogo = EscolherNivel | EmAndamento | PerdeuJogo | GanhouJogo deriving Eq

data Menu = MenuInicial | ModoJogo EstadoJogo deriving Eq

type Nivel = Int

data ImmutableTowers = ImmutableTowers { jogoAtual :: Jogo, 
                                         menu :: Menu, 
                                         imagens :: [(String,Picture)],
                                         torreSelecionadaLoja :: Maybe Torre,
                                         infoTorre :: Maybe Torre,
                                         nivelAtual :: Nivel -- ^ nivel máximo do jogo em que o inimigo pode entrar no momento
                                    }