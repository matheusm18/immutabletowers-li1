{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Matheus Henrique Monteiro da Silva Azevedo <a111430@alunos.uminho.pt>
              Francisco Luciano Martins <a111775@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425


dist :: Posicao -> Posicao -> Float
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance _ [] = []
inimigosNoAlcance torre inimigos = filter (\i -> dist (x,y) (posicaoInimigo i) <= r) inimigos
    where (x,y) = posicaoTorre torre
          r = alcanceTorre torre


verificaGelo :: [Projetil] -> Bool
verificaGelo listaprojetil = any (\p -> tipoProjetil p == Gelo) listaprojetil

verificaFogo:: [Projetil] -> Bool
verificaFogo listaprojetil = any (\p -> tipoProjetil p == Fogo) listaprojetil

verificaResina:: [Projetil] -> Bool
verificaResina listaprojetil = any (\p -> tipoProjetil p == Resina) listaprojetil

dobraDuracao :: Projetil -> Duracao
dobraDuracao (Projetil _ Infinita) = Infinita
dobraDuracao (Projetil _ (Finita d)) = Finita (2*d)

somaDuracao :: Projetil -> Projetil -> Duracao
somaDuracao (Projetil _ Infinita) (Projetil _ _ ) = Infinita
somaDuracao (Projetil _ _) (Projetil _ Infinita) = Infinita
somaDuracao (Projetil _ (Finita d1)) (Projetil _ (Finita d2)) = Finita (d1 + d2)

-- A lista de projéteis se não for vazia pode conter ou apenas Fogo, ou conter Resina com Gelo, ou Gelo com Resina.
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
            then p : [Projetil Gelo (somaDuracao pinc pg)]
            else pinc : [p] -- só existia resina na lista.
            where pg = head rp
         (Resina, Fogo) -> [Projetil Fogo (dobraDuracao p)]
         (Resina, Gelo) ->
            if verificaResina rp
            then p : [Projetil Resina (somaDuracao pinc pr)]
            else pinc : [p] -- se não houver resina, então só existia gelo na lista.
            where pr = head rp
         (Resina, Resina) -> (Projetil Resina (somaDuracao pinc p)) : rp

atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo Torre {danoTorre = dano, projetilTorre = projetil} i
    = Inimigo {vidaInimigo = max 0 ((vidaInimigo i) - dano), 
               projeteisInimigo = atualizaProjeteis projetil (projeteisInimigo i), 
               posicaoInimigo = posicaoInimigo i,
               direcaoInimigo = direcaoInimigo i,
               butimInimigo = butimInimigo i,
               velocidadeInimigo = velocidadeInimigo i,
               ataqueInimigo = ataqueInimigo i}

ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal  inimigos =
    case ondasPortal portal of
        [] -> (portal, inimigos)
        (onda:restoondas) -> 
            case inimigosOnda onda of
                [] -> (portal {ondasPortal = restoondas}, inimigos)
                (i:r) -> let novaOnda = Onda {inimigosOnda = r}
                             novoPortal = Portal {ondasPortal = novaOnda : restoondas}
                         in (novoPortal, inimigos ++ [i])

terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo (jogo) || perdeuJogo (jogo)

ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = vida > 0 && null ondas && null inimigos
        where
            vida = vidaBase (baseJogo jogo)
            ondas = concatMap (ondasPortal) (portaisJogo (jogo))
            inimigos = inimigosJogo (jogo)

perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vida <= 0
        where
            vida = vidaBase (baseJogo jogo)