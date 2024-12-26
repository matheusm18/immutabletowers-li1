import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers

-- Função principal para desenhar o mapa
desenha :: Mapa -> Picture
desenha mapa = Pictures $ concatMap desenhaLinha (zip [0..] mapa)

desenhaLinha :: (Int, [Terreno]) -> [Picture]
desenhaLinha (y, linha) = concatMap (desenhaChao y) (zip [0..] linha)

desenhaChao :: Int -> (Int, Terreno) -> [Picture]
desenhaChao y (x, terreno) = [desenhaTerreno terreno (posicaoCentro x y)]

-- Calcula a posição do centro de um quadrado no mapa
posicaoCentro :: Int -> Int -> (Float, Float)
posicaoCentro x y = (fromIntegral x + 0.5, fromIntegral y + 0.5)

-- Função para desenhar cada tipo de terreno
desenhaTerreno :: Terreno -> Posicao -> Picture
desenhaTerreno Agua (x, y) = desenhaChaoBase (x, y) (makeColorI 154 209 221 255)
desenhaTerreno Relva (x, y) = desenhaChaoBase (x, y) (makeColorI 91 142 80 255)
desenhaTerreno Terra (x, y) = desenhaChaoBase (x, y) (makeColorI 186 140 93 255)

desenhaChaoBase :: Posicao -> Color -> Picture
desenhaChaoBase (x, y) color = Color color $ translate (x * w) (-y * h) $ rectangleSolid w h

w :: Float
w = 40

h :: Float
h = 40

mapa01 :: [[Terreno]]
mapa01 =
    [ [t,t,a,r,r,r,r,r],
      [a,t,a,a,r,t,t,t],
      [a,t,a,a,r,t,r,a],
      [a,t,a,a,r,t,r,a],
      [t,t,a,a,r,t,t,t],
      [t,a,a,a,r,r,r,t],
      [t,a,t,t,t,t,t,t],
      [t,t,t,a,r,r,r,a]
    ]
  where
    t = Terra
    r = Relva
    a = Agua

-- Função para contar o número de colunas no mapa
contaCol :: Mapa -> Int
contaCol [] = 0
contaCol (linha:_) = length linha

-- Função para contar o número de linhas no mapa
contaLinhas :: Mapa -> Int
contaLinhas = length

background :: Color
background = greyN 0.6

-- Função principal para exibir a janela
main :: IO ()
main = display
            (InWindow "Immutable Towers" (800, 600) (100, 100))
            background
            (desenha mapa01)
