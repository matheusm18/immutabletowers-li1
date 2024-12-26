module ImmutableTowers where

data Terreno = Relva | Agua | Terra deriving (Show, Eq)

type Posicao = (Float, Float)

-- | Moeda do jogo.
type Creditos = Int

type Mapa = [[Terreno]]

data Base = Base
  { -- | Vida da base. Quando esta chega a zero, o jogador perde o jogo.
    vidaBase :: Float,
    -- | Posição da base no mapa. A base deve estar sobre um terreno de terra.
    posicaoBase :: Posicao,
    -- | Balanço de créditos do jogador.
    creditosBase :: Creditos
  }
  deriving (Show)

data ImmutableTowers = ImmutableTowers Mapa Base