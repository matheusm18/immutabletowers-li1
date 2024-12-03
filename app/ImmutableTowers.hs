module ImmutableTowers where

data Terreno = Relva | Agua | Terra deriving (Show, Eq)
type Mapa = [[Terreno]]
data ImmutableTowers = ImmutableTowers Mapa