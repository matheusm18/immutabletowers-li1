{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Matheus Henrique Monteiro da Silva Azevedo <a111430@alunos.uminho.pt>
              Francisco Luciano Martins <a111775@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425


-- | Função auxiliar que dado uma posição e o mapa verifica qual o tipo de terreno nessa posição
tipoTerreno :: Posicao -> Mapa -> Maybe Terreno
tipoTerreno (x,y) mapa
    | y' < 0 || y' >= fromIntegral (length (mapa)) = Nothing
    | x' < 0 || x' >= fromIntegral (length (mapa !! y')) = Nothing
    | otherwise = Just ((mapa !! y') !! x')  -- Acessa o terreno na coordenada (x, y)
    where x' = floor x
          y' = floor y

-- | Função que verifica se em uma certa posição o tipo do terreno é Terra.
validaPosicao :: Posicao -> Mapa -> Bool
validaPosicao (x,y) mapa = case t of
    Just Terra -> True
    _ -> False
    where t = tipoTerreno (x,y) mapa

{-| Função que dada uma posição retorna as posições que estão em volta que são válidas (tem terreno com Terra)

Como a função validaPosição (que é utilizada dentro do filter) utiliza a função tipoTerreno, não é necessário verificar se as posições adjacentes são válidas.
     
-}   

posAdjacentes :: Posicao -> Mapa -> [Posicao]
posAdjacentes (x,y) mapa = filter (\(cx,cy) -> validaPosicao (cx,cy) mapa) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- | Função auxiliar que verifica se existe um caminho entre duas posições.
verificaCaminho :: Posicao -> Posicao -> Mapa -> Bool
verificaCaminho posinicial posfinal mapa = buscaCaminho [posinicial] []
  where
    buscaCaminho [] _ = False
    buscaCaminho (atual:fila) visitados
        | atual == posfinal    = True
        | elem atual visitados = buscaCaminho fila visitados  -- ^ Se a posição atual já foi visitada, passa a procurar para a próxima posição
        | otherwise            =
            let visitados' = atual : visitados  -- ^ Adiciona a posição atual à lista de visitados
                adjacentes = filter (`notElem` visitados') (posAdjacentes atual mapa) -- ^ Filtra as posições adjacentes válidas que ainda não foram visitadas
            in buscaCaminho (fila ++ adjacentes) visitados'

verificaSobreposicao :: Posicao -> [Torre] -> Base -> Bool
verificaSobreposicao (x,y) [] Base {posicaoBase = posBase} = (x,y) /= posBase
verificaSobreposicao (x,y) (Torre { posicaoTorre = (xt,yt)} :rl) base@(Base {posicaoBase = posBase})
            = (x,y) /= (xt,yt) && verificaSobreposicao (x,y) rl base


-- !!! não percebi muito bem como fazer e se era assim, tenta rever esta parte:
verificaOndas :: Portal -> [Onda] -> Bool
verificaOndas _ [] = True
verificaOndas portal ondas@(Onda {entradaOnda = temporestante} :rl) = length ondasiniciadas == 1 || length ondasiniciadas == 0
    where ondasiniciadas = filter (\onda -> temporestante == 0) ondas


-- | Função que verifica se o estado de jogo está válido para os Portais
validaPortais :: [Portal] -> Mapa -> Base -> [Torre] -> Bool
validaPortais [] _ _ _ = False -- ^ Verifica se há pelo menos um portal
validaPortais portais mapa base torres =
    all (\p -> validaPosicao (posicaoPortal p) mapa) portais &&   -- ^ Verifica se a posição de cada portal é válida
    all (\p -> verificaCaminho (posicaoPortal p) (posicaoBase base) mapa) portais && -- ^ Verifica se para cada portal há um caminho entre ele e a base
    all (\p -> verificaSobreposicao (posicaoPortal p) (torres) (base)) portais && -- ^ Verifica se para cada portal ele não está sobreposto a uma torre/base
    all (\p -> verificaOndas p (ondasPortal p)) portais -- ^ Verifica se para cada portal tem no máximo uma onda ativa

-- | Função principal que verifica todas as condições estabelecidas para o jogo
validaJogo :: Jogo -> Bool
validaJogo Jogo {baseJogo = base, portaisJogo = portais, torresJogo = torres, mapaJogo = mapa} =
    validaPortais portais mapa base torres