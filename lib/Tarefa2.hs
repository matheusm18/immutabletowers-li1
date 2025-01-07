{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Matheus Henrique Monteiro da Silva Azevedo <a111430@alunos.uminho.pt>
              Francisco Luciano Martins <a111775@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

-- | Função auxiliar que calcula a distância entre duas posições.
dist :: Posicao -> Posicao -> Float
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

{- Função auxiliar que recebe uma torre e a lista de inimigos presentes no jogo e 
retorna a lista de inimigos que estão no alcance da torre. -}

inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance _ [] = []
inimigosNoAlcance torre inimigos = filter (\i -> dist (x,y) (posicaoInimigo i) <= r) inimigos
    where (x,y) = posicaoTorre torre
          r = alcanceTorre torre

-- | Função auxiliar que verifica se há projéteis do tipo Gelo na lista de projéteis.
verificaGelo :: [Projetil] -> Bool
verificaGelo listaprojetil = any (\p -> tipoProjetil p == Gelo) listaprojetil


-- | Função auxiliar que verifica se há projéteis do tipo Fogo na lista de projéteis.
verificaFogo :: [Projetil] -> Bool
verificaFogo listaprojetil = any (\p -> tipoProjetil p == Fogo) listaprojetil


-- | Função auxiliar que verifica se há projéteis do tipo Resina na lista de projéteis.
verificaResina :: [Projetil] -> Bool
verificaResina listaprojetil = any (\p -> tipoProjetil p == Resina) listaprojetil

-- | Função auxiliar que dobra a duração de um projétil.
dobraDuracao :: Projetil -> Duracao
dobraDuracao (Projetil _ Infinita) = Infinita
dobraDuracao (Projetil _ (Finita d)) = Finita (2*d)

-- | Função auxiliar que soma a duração de dois projéteis.
somaDuracao :: Projetil -> Projetil -> Duracao
somaDuracao (Projetil _ Infinita) (Projetil _ _ ) = Infinita
somaDuracao (Projetil _ _) (Projetil _ Infinita) = Infinita
somaDuracao (Projetil _ (Finita d1)) (Projetil _ (Finita d2)) = Finita (d1 + d2)

{- Função que recebe um projetil incidente e uma outra lista de projeteis ativos e retorna a lista de projeteis normalizada.

Isto é, normaliza de acordo com as sinergias e pela ordem de precedência referida no enunciado do projeto.

A lista de projetéis contém no máximo 2 elementos, as combinações possiveis na lista (se esta não for vazia) são:

* Fogo
* Gelo
* Resina
* Resina e Gelo
-}

atualizaProjeteis :: Projetil -> [Projetil] -> [Projetil]
atualizaProjeteis p [] = [p]
atualizaProjeteis pinc (p:rp) = 
    case (tipoProjetil pinc, tipoProjetil p) of
         (Fogo, Gelo) -> rp
         (Fogo, Resina) ->
            if verificaGelo rp
            then [p] -- deixa apenas a resina
            else [Projetil Fogo (dobraDuracao pinc)] -- só havia resina na lista.
         (Fogo, Fogo) -> [Projetil Fogo (somaDuracao pinc p)]
         (Gelo, Fogo) -> rp
         (Gelo, Gelo) -> (Projetil Gelo (somaDuracao pinc p)) : rp
         (Gelo, Resina) ->
            if verificaGelo rp
            then (Projetil Gelo (somaDuracao pinc pg)) : [p]
            else pinc : [p] -- só existia resina na lista.
            where pg = head rp
         (Resina, Fogo) -> [Projetil Fogo (dobraDuracao p)]
         (Resina, Gelo) ->
            if verificaResina rp
            then (Projetil Resina (somaDuracao pinc pr)) : [p]
            else pinc : [p] -- se não houver resina, então só existia gelo na lista.
            where pr = head rp
         (Resina, Resina) -> (Projetil Resina (somaDuracao pinc p)) : rp

{- Função que dada uma torre e o inimigo que será atingido pela torre, retorna o inimigo
com a vida atualizada e com a lista de projetéis atualizada.

No caso da resina, a velocidade do inimigo também é atualizada. -}

atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo Torre {danoTorre = dano, projetilTorre = projetil} i
    = case tipoProjetil projetil of 
        Resina -> i {vidaInimigo = vidaInimigo i - dano, 
                     projeteisInimigo = atualizaProjeteis projetil (projeteisInimigo i), 
                     velocidadeInimigo = if verificaFogo (projeteisInimigo i) -- ^ se existia fogo, não haverá resina
                                         then velocidadeInimigo i
                                         else if verificaResina (projeteisInimigo i) -- ^ se existia resina, a velocidade já estava reduzida
                                         then velocidadeInimigo i 
                                         else velocidadeInimigo i / 2}
        _ -> i {vidaInimigo = vidaInimigo i - dano, projeteisInimigo = atualizaProjeteis projetil (projeteisInimigo i)}

{- Função que dada uma torre e a lista de inimigos ativos no jogo, 
retorna uma tupla com o portal atualizado após a ativação do inimigo e a nova lista de inimigos ativos no jogo (adição do inimigo ativado) 
-}

ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal inimigos =
    case ondasPortal portal of
        [] -> (portal, inimigos)
        (onda:restoondas) -> 
            case inimigosOnda onda of
                [] -> (portal {ondasPortal = restoondas}, inimigos)
                (i:r) -> let novaOnda = onda {inimigosOnda = r, tempoOnda = cicloOnda onda}
                             novoPortal = portal {ondasPortal = (novaOnda : restoondas)}
                         in (novoPortal, i : inimigos)

-- | Função que dado um jogo verifica se o jogo terminou, i.e, se o jogador ganhou ou perdeu.
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo (jogo) || perdeuJogo (jogo)

{- Função que dado um jogo verifica se o jogador ganhou.

O jogador ganha quando a vida da base é positiva e não existam ondas nem inimigos ativos no jogo.
-}

ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = vida > 0 && null ondas && null inimigos
        where
            vida = vidaBase (baseJogo jogo)
            ondas = concatMap (ondasPortal) (portaisJogo (jogo))
            inimigos = inimigosJogo (jogo)

{- Função que dado um jogo verifica se o jogador perdeu.

O jogador perde quando a vida da base deixa de ser positiva.
-}

perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vida <= 0
        where
            vida = vidaBase (baseJogo jogo)