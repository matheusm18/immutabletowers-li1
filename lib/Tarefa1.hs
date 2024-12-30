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
validaPosicaoTerra :: Posicao -> Mapa -> Bool
validaPosicaoTerra (x,y) mapa = case t of
    Just Terra -> True
    _ -> False
    where t = tipoTerreno (x,y) mapa

{-| Função que dada uma posição retorna as posições que estão em volta que são válidas (tem terreno com Terra)

Como a função validaPosicaoTerra (que é utilizada dentro do filter) utiliza a função tipoTerreno, não é necessário verificar se as posições adjacentes são válidas.
     
-}   

posAdjacentes :: Posicao -> Mapa -> [Posicao]
posAdjacentes (x,y) mapa = filter (\(cx,cy) -> validaPosicaoTerra (cx,cy) mapa) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

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


-- Função auxiliar que dada uma lista de ondas de um dado portal verifica se há no máximo uma onda iniciada.
verificaOndas :: [Onda] -> Bool
verificaOndas [] = True
verificaOndas ondas = length ondasiniciadas == 1 || length ondasiniciadas == 0
    where ondasiniciadas = filter (\onda -> (entradaOnda onda) == 0) ondas

-- | Função que verifica se o estado de jogo está válido para os Portais
validaPortais :: [Portal] -> Mapa -> Base -> [Torre] -> Bool
validaPortais [] _ _ _ = False -- ^ Verifica se há pelo menos um portal
validaPortais portais mapa base torres =
    all (\p -> validaPosicaoTerra (posicaoPortal p) mapa) portais &&   -- ^ Verifica se a posição de cada portal é válida
    all (\p -> verificaCaminho (posicaoPortal p) (posicaoBase base) mapa) portais && -- ^ Verifica se para cada portal há um caminho entre ele e a base
    all (\p -> verificaSobreposicao (posicaoPortal p) (torres) (base)) portais && -- ^ Verifica se para cada portal ele não está sobreposto a uma torre/base
    all (\p -> verificaOndas (ondasPortal p)) portais -- ^ Verifica se para cada portal tem no máximo uma onda ativa


-- | Função que dada a posição do portal e um inimigo, verifica se o inimigo cumpre as restrições da alínea a)
verificaRestricoes :: Posicao -> Inimigo -> Bool
verificaRestricoes posPortal Inimigo {posicaoInimigo = pos, vidaInimigo = vida, projeteisInimigo = lprojeteis}
            = posPortal == pos && vida > 0 && (lprojeteis == [])

-- | Função que dada uma onda em que cada onda tem uma lista de inimigos, concatena estas listas e retorna uma lista de inimigos geral
concatenaInimigos :: [Onda] -> [Inimigo]
concatenaInimigos [] = []
concatenaInimigos (onda:rl) = (inimigosOnda onda) ++ concatenaInimigos rl

-- | Função auxiliar que verifica se um inimigo está sobreposto a alguma torre
verificaSobreposicaoInimigoTorres :: [Torre] -> Inimigo -> Bool
verificaSobreposicaoInimigoTorres torres inimigo = all (\torre -> (posicaoTorre torre) /= (posicaoInimigo inimigo)) torres

-- | Função auxiliar que verifica se a velocidade de um inimigo é válida (não é negativa)
validaVelocidade :: Inimigo -> Bool
validaVelocidade inimigo = (velocidadeInimigo (inimigo)) >= 0

-- | Função que verifica se a lista de projéteis ativos é válida
validaProjeteis :: [Projetil] -> Bool
validaProjeteis lp = length(lfogos) <=1 && 
                     length(lgelos) <=1 && 
                     length(lresinas) <=1 &&
                     (if length(lfogos) == 1 && length(lresinas) /= 0 then False
                      else if length(lresinas) == 1 && length(lfogos) /= 0 then False
                      else if length(lfogos) == 1 && length(lgelos) /= 0 then False
                      else if length (lgelos) == 1 && length(lfogos) /= 0 then False
                      else True)
                        where listatiposp@(tp1:rtps) = map (tipoProjetil) lp
                              lfogos = filter (== Fogo) listatiposp
                              lgelos = filter (== Gelo) listatiposp
                              lresinas = filter (== Resina) listatiposp

-- | Função que verifica se todos os inimigos de todas as ondas cumprem as restrições da alínea a)
validaInimigosPortal :: Posicao -> [Onda] -> Bool
validaInimigosPortal posPortal ondasPortal =
    all (\i -> verificaRestricoes posPortal i) inimigosPortal
    where inimigosPortal = concatenaInimigos ondasPortal

-- | Função que verifica todas as restrições estabelecidas para os inimigos em jogo.
validaInimigosEmJogo :: [Inimigo] -> [Torre] -> Mapa -> Bool
validaInimigosEmJogo inimigosemjogo torres mapa = 
    all (\i -> validaPosicaoTerra (posicaoInimigo i) mapa) inimigosemjogo &&
    all (\i -> verificaSobreposicaoInimigoTorres torres i) inimigosemjogo &&
    all (\i -> validaVelocidade i) inimigosemjogo &&
    all (\i -> validaProjeteis (projeteisInimigo i)) inimigosemjogo

-- | Função que verifica se em uma certa posição o tipo do terreno é Relva.
validaPosicaoRelva :: Posicao -> Mapa -> Bool
validaPosicaoRelva (x,y) mapa = case t of
    Just Relva -> True
    _ -> False
    where t = tipoTerreno (x,y) mapa

validaAlcanceTorre :: Torre -> Bool
validaAlcanceTorre torre = (alcanceTorre (torre)) > 0

validaRajadaTorre :: Torre -> Bool
validaRajadaTorre torre = (rajadaTorre (torre)) > 0

validaCicloTorre :: Torre -> Bool
validaCicloTorre torre = (cicloTorre (torre)) >= 0

verificaSobreposicaoTorres :: [Torre] -> [Torre] -> Bool
verificaSobreposicaoTorres  [] torres = True
verificaSobreposicaoTorres (t:r) l  = length (filter (== posicaoTorre t) (map (posicaoTorre) l)) == 1 && verificaSobreposicaoTorres r l

validaTorres :: [Torre] -> Mapa -> Bool
validaTorres torres mapa = 
    all (\torre -> validaPosicaoRelva (posicaoTorre torre) mapa) torres && -- ^ Verifica se as torres estão sobre relva
    all (\torre -> validaAlcanceTorre torre) torres &&
    all (\torre -> validaRajadaTorre torre) torres &&
    all (\torre -> validaCicloTorre torre) torres &&
    verificaSobreposicaoTorres torres torres

validaCreditoBase :: Base -> Bool
validaCreditoBase base = (creditosBase (base)) >= 0

verificaSobreposicaoBase :: Posicao -> [Torre] -> [Portal] -> Bool
verificaSobreposicaoBase (x,y) [] [] = True
verificaSobreposicaoBase (x,y) [] (Portal { posicaoPortal = posPortal} :rp)= (x,y) /= posPortal && verificaSobreposicaoBase (x,y) [] rp
verificaSobreposicaoBase (x,y) (Torre { posicaoTorre = posTorre} :rl) [] = (x,y) /= posTorre && verificaSobreposicaoBase (x,y) rl []
verificaSobreposicaoBase (x,y) (Torre { posicaoTorre = posTorre} :rl) (Portal {posicaoPortal = posPortal} :rp)
            = verificaSobreposicaoBase (x,y) (Torre { posicaoTorre = posTorre} :rl) [] && verificaSobreposicaoBase (x,y) [] (Portal {posicaoPortal = posPortal} :rp)

validaBase :: Base -> Mapa -> [Torre] -> [Portal] -> Bool
validaBase base mapa torres portais =
    validaPosicaoTerra (posicaoBase base) mapa &&
    validaCreditoBase base &&
    verificaSobreposicaoBase (posicaoBase base) torres portais 

-- | Função principal que verifica todas as condições estabelecidas para o jogo
validaJogo :: Jogo -> Bool
validaJogo Jogo {baseJogo = base, portaisJogo = portais, torresJogo = torres, mapaJogo = mapa, inimigosJogo = inimigos} =
    validaPortais portais mapa base torres &&
    all (\p -> validaInimigosPortal (posicaoPortal p) (ondasPortal p)) portais && -- ^ Valida inimigos por lançar
    validaInimigosEmJogo inimigos torres mapa && -- ^ Valida inimigos em jogo
    validaTorres torres mapa &&
    validaBase base mapa torres portais

