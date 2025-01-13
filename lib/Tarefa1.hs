{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Matheus Henrique Monteiro da Silva Azevedo <a111430@alunos.uminho.pt>
              Francisco Luciano Martins <a111775@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425

{-| Função auxiliar que dada uma posição e o mapa, verifica qual o tipo de terreno nessa posição.

== Exemplos:

>>> tipoTerreno (1,1) [[Terra,Relva],[Relva,Agua]]
Just Agua

>>> tipoTerreno (1.5,0.5) [[Terra,Relva],[Relva,Agua]]
Just Relva
-}

tipoTerreno :: Posicao -> Mapa -> Maybe Terreno
tipoTerreno (x,y) mapa
    | y' < 0 || y' >= fromIntegral (length (mapa)) = Nothing
    | x' < 0 || x' >= fromIntegral (length (mapa !! y')) = Nothing
    | otherwise = Just ((mapa !! y') !! x')  -- ^ acessa o terreno na coordenada (x, y)
    where x' = floor x
          y' = floor y

{-| Função que verifica se em uma certa posição o tipo do terreno é Terra.

== Exemplos:

>>> validaPosicaoTerra (0,0) [[Terra,Relva],[Terra,Agua]]
True

>>> validaPosicaoTerra (1,1) [[Terra,Relva],[Terra,Agua]]
False
-}

validaPosicaoTerra :: Posicao -> Mapa -> Bool
validaPosicaoTerra (x,y) mapa = case t of
    Just Terra -> True
    _ -> False
    where t = tipoTerreno (x,y) mapa

{-| Função que verifica se em uma certa posição o tipo do terreno é Relva.

== Exemplos:

>>> validaPosicaoRelva (1.5,0) [[Terra,Relva],[Terra,Agua]]
True

>>> validaPosicaoRelva (0,0) [[Terra,Relva],[Terra,Agua]]
False
-}

validaPosicaoRelva :: Posicao -> Mapa -> Bool
validaPosicaoRelva (x,y) mapa = case t of
    Just Relva -> True
    _ -> False
    where t = tipoTerreno (x,y) mapa

{-| Função que dada uma posição retorna as posições que estão em volta cujo terreno é Terra.

Como a função validaPosicaoTerra (que é utilizada dentro do filter) utiliza a função tipoTerreno, não é necessário verificar se as posições adjacentes são válidas.

== Exemplos:

>>> posAdjacentes (0.5,0.5) [[Terra,Terra,Agua],[Relva,Terra,Terra], [Relva,Terra,Terra]]
[(1.5,0.5)]

>>> posAdjacentes (1.5,1.5) [[Terra,Terra,Agua],[Relva,Terra,Terra],[Relva,Terra,Terra]]
[(2.5,1.5),(1.5,0.5),(1.5,2.5)]
-}   

posAdjacentes :: Posicao -> Mapa -> [Posicao]
posAdjacentes (x,y) mapa = filter (\(cx,cy) -> validaPosicaoTerra (cx,cy) mapa) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

{-| Função que verifica se existe um caminho entre duas posições.

== Exemplos:

>>> verificaCaminho (0.5,0.5) (2.5,2.5) [[Terra,Terra,Agua],[Relva,Terra,Terra],[Relva,Relva,Terra]]
True

>>> verificaCaminho (0.5,0.5) (1.5,1.5) [[Terra,Agua],[Agua,Terra]]
False
-}

verificaCaminho :: Posicao -> Posicao -> Mapa -> Bool
verificaCaminho posinicial posfinal mapa = buscaCaminho [posinicial] []
  where
    buscaCaminho [] _ = False
    buscaCaminho (atual:fila) visitados
        | atual == posfinal    = True
        | elem atual visitados = buscaCaminho fila visitados  -- ^ se a posição atual já foi visitada, passa a procurar para as próximas posições
        | otherwise            =
            let visitados' = atual : visitados  -- ^ adiciona a posição atual à lista de visitados
                adjacentes = filter (`notElem` visitados') (posAdjacentes atual mapa) -- ^ filtra as posições adjacentes válidas que ainda não foram visitadas
            in buscaCaminho (fila ++ adjacentes) visitados'

{-| Função que verifica se existem torres ou base numa dada posição.

Retorna True quando não há sobreposições (torres ou base na posição), retorna False caso contrário.

== Exemplos:

>>> verificaSobreposicao (1,4) [] (Base {posicaoBase = (2,7)})
True

>>> verificaSobreposicao (2,7) [Torre {posicaoTorre = (2,4)}, Torre {posicaoTorre = (2,7)}] (Base {posicaoBase = (1,3)})
False
-}

verificaSobreposicao :: Posicao -> [Torre] -> Base -> Bool
verificaSobreposicao (x,y) [] Base {posicaoBase = posBase} = (x,y) /= posBase
verificaSobreposicao (x,y) ((Torre {posicaoTorre = (xt,yt)}):rl) base
            = (x,y) /= (xt,yt) && verificaSobreposicao (x,y) rl base -- ^ recursão para verificar todas as torres


{-| Função que dada a lista de ondas de um dado portal verifica se há no máximo uma onda iniciada.

Retorna True quando há no máximo uma onda iniciada, False caso contrário.

== Exemplos:

>>> verificaOndas []
True

>>> verificaOndas [Onda {entradaOnda = 0}, Onda {entradaOnda = 0}, Onda {entradaOnda = 7}]
False
-}

verificaOndas :: [Onda] -> Bool
verificaOndas [] = True
verificaOndas ondas = length ondasiniciadas == 1 || length ondasiniciadas == 0
    where ondasiniciadas = filter (\onda -> (entradaOnda onda) == 0) ondas

{-| Função que verifica se o estado de jogo está válido para os Portais (cumprem todos os critérios).

== Exemplos:

>>> validaPortais [Portal {posicaoPortal = (0.5,0.5), ondasPortal = [Onda {entradaOnda = 0}, Onda {entradaOnda = 7}]}] [[Terra, Terra],[Relva,Terra]] (Base {posicaoBase = (1.5, 1.5)}) [Torre {posicaoTorre = (0.5, 1.5)}]
True

>>> validaPortais [] [[Terra,Agua],[Agua,Terra]] (Base {posicaoBase = (1,1)}) [Torre {posicaoTorre = (0,0)}]
False
-}

validaPortais :: [Portal] -> Mapa -> Base -> [Torre] -> Bool
validaPortais [] _ _ _ = False -- ^ verifica se há pelo menos um portal
validaPortais portais mapa base torres =
    all (\p -> validaPosicaoTerra (posicaoPortal p) mapa) portais &&   -- ^ verifica se a posição de cada portal é válida
    all (\p -> verificaCaminho (posicaoPortal p) (posicaoBase base) mapa) portais && -- ^ verifica se para cada portal há um caminho entre ele e a base
    all (\p -> verificaSobreposicao (posicaoPortal p) (torres) (base)) portais && -- ^ verifica se para cada portal ele não está sobreposto a uma torre/base
    all (\p -> verificaOndas (ondasPortal p)) portais -- ^ verifica se para cada portal tem no máximo uma onda ativa


{-| Função que dada a posição do portal e um inimigo deste portal(por lançar), verifica se o inimigo cumpre as restrições da alínea a).

== Exemplos:

>>> verificaRestricoes (4,5) Inimigo {posicaoInimigo = (4,5), vidaInimigo = 100, projeteisInimigo = []}
True

>>> verificaRestricoes (4,5) Inimigo {posicaoInimigo = (2,1), vidaInimigo = 100, projeteisInimigo = []}
False
-}

verificaRestricoes :: Posicao -> Inimigo -> Bool
verificaRestricoes posPortal Inimigo {posicaoInimigo = posI, vidaInimigo = vida, projeteisInimigo = lprojeteis}
            = posPortal == posI && vida > 0 && (lprojeteis == [])

{-| Função auxiliar que dada uma lista de ondas em que cada onda tem uma lista de inimigos, 
concatena estas listas de inimigos e retorna a lista de inimigos concatenada.

== Exemplos:

>>> concatenaInimigos []
[]

>>> concatenaInimigos [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1,0), direcaoInimigo = Sul, vidaInimigo = 100, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}]},Onda {inimigosOnda = [Inimigo {posicaoInimigo = (2,7), direcaoInimigo = Este, vidaInimigo = 75, velocidadeInimigo = 2.0, ataqueInimigo = 15, butimInimigo = 10, projeteisInimigo = [], tipoInimigo = Blindado}]}]
[Inimigo {posicaoInimigo = (1.0,0.0), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal},Inimigo {posicaoInimigo = (2.0,7.0), direcaoInimigo = Este, vidaInimigo = 75.0, velocidadeInimigo = 2.0, ataqueInimigo = 15.0, butimInimigo = 10, projeteisInimigo = [], tipoInimigo = Blindado}]
-}

concatenaInimigos :: [Onda] -> [Inimigo]
concatenaInimigos [] = []
concatenaInimigos (onda:rl) = (inimigosOnda onda) ++ concatenaInimigos rl

{-| Função auxiliar que verifica se um inimigo está sobreposto a alguma torre.

Retorna True quando o inimigo não está sobreposto a nenhuma torre, False caso contrário.

== Exemplos:

>>> verificaSobreposicaoInimigoTorres [] (Inimigo {posicaoInimigo = (5,3)})
True

>>> verificaSobreposicaoInimigoTorres [Torre {posicaoTorre = (7,3)}] (Inimigo {posicaoInimigo = (8,5)})
True

>>> verificaSobreposicaoInimigoTorres [Torre {posicaoTorre = (4,5)}] (Inimigo {posicaoInimigo = (4,5)})
False
-}

verificaSobreposicaoInimigoTorres :: [Torre] -> Inimigo -> Bool
verificaSobreposicaoInimigoTorres torres inimigo = all (\torre -> (posicaoTorre torre) /= (posicaoInimigo inimigo)) torres

{-| Função auxiliar que verifica se a velocidade de um inimigo é válida (não é negativa).

== Exemplos:

>>> validaVelocidade Inimigo {velocidadeInimigo = 1}
True

>>> validaVelocidade Inimigo {velocidadeInimigo = -2}
False
-}

validaVelocidade :: Inimigo -> Bool
validaVelocidade inimigo = (velocidadeInimigo (inimigo)) >= 0

{-| Função que verifica se a lista de projéteis ativos é válida.

Isto é, não pode conter mais do que um projétil do mesmo tipo e não pode conter simultaneamente Fogo e Resina, 
nem Fogo e Gelo.

== Exemplos:

>>> validaProjeteis [Projetil Gelo (Finita 5), Projetil Resina (Finita 3)]
True

>>> validaProjeteis [Projetil Fogo (Finita 5), Projetil Gelo (Finita 3)]
False
-}

validaProjeteis :: [Projetil] -> Bool
validaProjeteis lp = length(lfogos) <=1 && 
                     length(lgelos) <=1 && 
                     length(lresinas) <=1 &&
                     (if length(lfogos) == 1 && (length(lresinas) /= 0 || length(lgelos) /= 0) then False
                      else if length(lresinas) == 1 && length(lfogos) /= 0 then False
                      else if length (lgelos) == 1 && length(lfogos) /= 0 then False
                      else True)
                        where listaTiposP = map (tipoProjetil) lp
                              lfogos = filter (== Fogo) listaTiposP
                              lgelos = filter (== Gelo) listaTiposP
                              lresinas = filter (== Resina) listaTiposP

{-| Função que verifica se todos os inimigos de todas as ondas de um portal cumprem as restrições da alínea a).

== Exemplos:

>>> validaInimigosPortal (1,0) [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1,0), vidaInimigo = 100, projeteisInimigo = []}]}]
True

>>> validaInimigosPortal (1,0) [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (2,7), vidaInimigo = 100, projeteisInimigo = []}]}]
False
-}

validaInimigosPortal :: Posicao -> [Onda] -> Bool
validaInimigosPortal posPortal ondasPortal =
    all (\i -> verificaRestricoes posPortal i) inimigosPortal
    where inimigosPortal = concatenaInimigos ondasPortal

{-| Função que verifica todas as restrições estabelecidas para os inimigos em jogo.

== Exemplos:

>>> validaInimigosEmJogo [Inimigo {posicaoInimigo = (1.5,0), velocidadeInimigo = 3, projeteisInimigo = []}] [] [[Agua, Terra], [Terra, Relva]]
True

>>> validaInimigosEmJogo [Inimigo {posicaoInimigo = (0,0), velocidadeInimigo = 3, projeteisInimigo = []}] [] [[Agua, Terra], [Terra, Relva]]
False
-}

validaInimigosEmJogo :: [Inimigo] -> [Torre] -> Mapa -> Bool
validaInimigosEmJogo inimigosemjogo torres mapa = 
    all (\i -> validaPosicaoTerra (posicaoInimigo i) mapa) inimigosemjogo &&
    all (\i -> verificaSobreposicaoInimigoTorres torres i) inimigosemjogo &&
    all (\i -> validaVelocidade i) inimigosemjogo &&
    all (\i -> validaProjeteis (projeteisInimigo i)) inimigosemjogo

{-| Função que verifica se a torre tem um alcance válido (valor positivo).

== Exemplos:

>>> validaAlcanceTorre (Torre {alcanceTorre = 2})
True

>>> validaAlcanceTorre (Torre {alcanceTorre = 0})
False
-}

validaAlcanceTorre :: Torre -> Bool
validaAlcanceTorre torre = (alcanceTorre (torre)) > 0

{-| Função que verifica se a torre tem uma rajada válida (valor positivo).

== Exemplos:

>>> validaRajadaTorre (Torre {rajadaTorre = 2})
True

>>> validaRajadaTorre (Torre {rajadaTorre = 0})
False
-}

validaRajadaTorre :: Torre -> Bool
validaRajadaTorre torre = (rajadaTorre (torre)) > 0

{-| Função que verifica se a torre tem um ciclo válido (valor não negativo).

== Exemplos:

>>> validaCicloTorre (Torre {cicloTorre = 2})
True

>>> validaCicloTorre (Torre {cicloTorre = -1})
False
-}

validaCicloTorre :: Torre -> Bool
validaCicloTorre torre = (cicloTorre (torre)) >= 0

{-| Função que verifica se as torres não estão sobrepostas.

Retorna True quando as torres não estão sobrepostas, False caso contrário.

== Exemplos:

>>> verificaSobreposicaoTorres [Torre {posicaoTorre = (1,0)}, Torre {posicaoTorre = (3,2)}] [Torre {posicaoTorre = (1,0)}, Torre {posicaoTorre = (3,2)}]
True

>>> verificaSobreposicaoTorres [Torre {posicaoTorre = (0,0)}, Torre {posicaoTorre = (0,0)}] [Torre {posicaoTorre = (0,0)}, Torre {posicaoTorre = (0,0)}]
False
-}

verificaSobreposicaoTorres :: [Torre] -> [Torre] -> Bool
verificaSobreposicaoTorres  [] _ = True
verificaSobreposicaoTorres (t:r) l  = length (filter (== posicaoTorre t) (posicoesTorres)) == 1 && verificaSobreposicaoTorres r l
    where posicoesTorres = map (posicaoTorre) l

{-| Função que verifica se o estado de jogo está válido para os Torres (cumprem todos os critérios).

== Exemplos:

>>> validaTorres [Torre {posicaoTorre = (0,0), alcanceTorre = 2, rajadaTorre = 2, cicloTorre = 2}, Torre {posicaoTorre = (1,0), alcanceTorre = 2, rajadaTorre = 2, cicloTorre = 2}] [[Relva, Relva], [Agua, Relva]]
True

>>> validaTorres [Torre {posicaoTorre = (3,0), alcanceTorre = 0, rajadaTorre = 2, cicloTorre = 2}, Torre {posicaoTorre = (1,0), alcanceTorre = 2, rajadaTorre = 2, cicloTorre = 2}] [[Relva, Relva], [Agua, Relva]]
False
-}

validaTorres :: [Torre] -> Mapa -> Bool
validaTorres torres mapa = 
    all (\torre -> validaPosicaoRelva (posicaoTorre torre) mapa) torres && -- ^ verifica se as torres estão sobre relva
    all (\torre -> validaAlcanceTorre torre) torres &&
    all (\torre -> validaRajadaTorre torre) torres &&
    all (\torre -> validaCicloTorre torre) torres &&
    verificaSobreposicaoTorres torres torres

{-| Função que verifica se a base tem um crédito válido (valor não negativo).

== Exemplos:

>>> validaCreditoBase (Base {creditosBase = 100})
True

>>> validaCreditoBase (Base {creditosBase = -15})
False
-}

validaCreditoBase :: Base -> Bool
validaCreditoBase base = (creditosBase (base)) >= 0

{-| Função que dada a posição da base, a lista das torres e a lista dos portais, 
verifica se a base não está sobreposta a torres ou portais.

Retorna True quando a base não está sobreprosta a torres ou portais, False caso contrário.

== Exemplos:

>>> verificaSobreposicaoBase (7,5) [] []
True

>>> verificaSobreposicaoBase (7,5) [Torre {posicaoTorre = (7,5)}] [Portal {posicaoPortal = (1,1)}]
False
-}

verificaSobreposicaoBase :: Posicao -> [Torre] -> [Portal] -> Bool
verificaSobreposicaoBase posBase torres portais =
    all (\torre -> (posicaoTorre torre /= posBase)) torres && -- ^ verifica para cada torre
    all (\portal -> (posicaoPortal portal /= posBase)) portais -- ^ verifica para cada portal

{-| Função que verifica se o estado de jogo está válido para a Base (cumpre todos os critérios).

== Exemplos:

>>> validaBase (Base {creditosBase = 100, posicaoBase = (1,0)}) [[Terra, Terra], [Agua, Agua]] [Torre {posicaoTorre = (7,5)}] [Portal {posicaoPortal = (1,1)}]
True

>>> validaBase (Base {creditosBase = -123, posicaoBase = (1.5,1.5)}) [[Terra, Terra], [Agua, Agua]] [Torre {posicaoTorre = (1.5,1.5)}] [Portal {posicaoPortal = (1,1)}]
False
-}

validaBase :: Base -> Mapa -> [Torre] -> [Portal] -> Bool
validaBase base mapa torres portais =
    validaPosicaoTerra (posicaoBase base) mapa &&
    validaCreditoBase base &&
    verificaSobreposicaoBase (posicaoBase base) torres portais 

{-| Função principal que verifica todas as condições estabelecidas para o jogo.

== Exemplos:

>>> validaJogo (Jogo {baseJogo = Base { vidaBase = 100.0, posicaoBase = (0.5, 0.5), creditosBase = 123}, portaisJogo = [Portal {posicaoPortal = (1.5, 0.5), ondasPortal = [Onda {inimigosOnda = [Inimigo (1.5, 0.5) Norte 10.0 1.0 5.0 10 [] Normal], cicloOnda = 5.0, tempoOnda = 2.0, entradaOnda = 10.0}]}], torresJogo = [], mapaJogo = [[Terra, Terra], [Relva, Relva]], inimigosJogo = [], lojaJogo = [], precoUpgrades = []})
True

>>> validaJogo (Jogo {baseJogo = Base { vidaBase = 100.0, posicaoBase = (40, 0), creditosBase = -123}, portaisJogo = [], torresJogo = [], mapaJogo = [[Terra, Terra], [Agua, Agua]], inimigosJogo = [], lojaJogo = [], precoUpgrades = []})
False
-}

validaJogo :: Jogo -> Bool
validaJogo Jogo {baseJogo = base, portaisJogo = portais, torresJogo = torres, mapaJogo = mapa, inimigosJogo = inimigos} =
    validaPortais portais mapa base torres &&
    all (\p -> validaInimigosPortal (posicaoPortal p) (ondasPortal p)) portais && -- ^ valida inimigos por lançar
    validaInimigosEmJogo inimigos torres mapa && -- ^ valida inimigos em jogo
    validaTorres torres mapa &&
    validaBase base mapa torres portais