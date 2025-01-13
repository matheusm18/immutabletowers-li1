{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Matheus Henrique Monteiro da Silva Azevedo <a111430@alunos.uminho.pt>
              Francisco Luciano Martins <a111775@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425

{-| Função auxiliar que calcula a distância entre duas posições.

== Exemplos:

>>> dist (0,0) (0,2)
2.0

>>> dist (0,0) (3,4)
5.0
-}

dist :: Posicao -> Posicao -> Float
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

{-| Função auxiliar que recebe uma torre e a lista de inimigos presentes no jogo e 
retorna a lista de inimigos que estão no alcance desta torre. 

== Exemplos:

>>> inimigosNoAlcance Torre {posicaoTorre = (0,0), alcanceTorre = 2} []
[]

>>> inimigosNoAlcance Torre {posicaoTorre = (0,0), alcanceTorre = 2} [Inimigo {posicaoInimigo = (1,0), direcaoInimigo = Sul, vidaInimigo = 100, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}]
[Inimigo {posicaoInimigo = (1.0,0.0), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}]
-}

inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance _ [] = []
inimigosNoAlcance torre inimigos = filter (\i -> dist (x,y) (posicaoInimigo i) <= r) inimigos
    where (x,y) = posicaoTorre torre
          r = alcanceTorre torre

{-| Função auxiliar que verifica se há projéteis do tipo Gelo na lista de projéteis.

== Exemplos:

>>> verificaGelo [Projetil Resina (Finita 5),Projetil Gelo (Finita 3)]
True

>>> verificaGelo []
False
-}

verificaGelo :: [Projetil] -> Bool
verificaGelo listaprojetil = any (\p -> tipoProjetil p == Gelo) listaprojetil

{-| Função auxiliar que verifica se há projéteis do tipo Fogo na lista de projéteis.

== Exemplos:

>>> verificaFogo [Projetil Fogo (Finita 5)]
True

>>> verificaFogo [Projetil Gelo (Finita 3)]
False
-}

verificaFogo :: [Projetil] -> Bool
verificaFogo listaprojetil = any (\p -> tipoProjetil p == Fogo) listaprojetil

{-| Função auxiliar que verifica se há projéteis do tipo Resina na lista de projéteis.

== Exemplos:

>>> verificaResina [Projetil Gelo (Finita 3), Projetil Resina (Finita 5)]
True

>>> verificaResina [Projetil Fogo (Finita 5)]
False
-}

verificaResina :: [Projetil] -> Bool
verificaResina listaprojetil = any (\p -> tipoProjetil p == Resina) listaprojetil

{-| Função auxiliar que dobra a duração de um projétil e devolve a duração dobrada.

== Exemplos:

>>> dobraDuracao (Projetil Fogo (Finita 5))
Finita 10.0

>>> dobraDuracao (Projetil Gelo Infinita)
Infinita
-}

dobraDuracao :: Projetil -> Duracao
dobraDuracao (Projetil _ Infinita) = Infinita
dobraDuracao (Projetil _ (Finita d)) = Finita (2*d)

{-| Função auxiliar que soma a duração de dois projéteis e devolve a duração somada.

== Exemplos:

>>> somaDuracao (Projetil Fogo Infinita) (Projetil Fogo (Finita 3))
Infinita

>>> somaDuracao (Projetil Gelo (Finita 3)) (Projetil Gelo (Finita 5))
Finita 8.0
-}

somaDuracao :: Projetil -> Projetil -> Duracao
somaDuracao (Projetil _ Infinita) (Projetil _ _ ) = Infinita
somaDuracao (Projetil _ _) (Projetil _ Infinita) = Infinita
somaDuracao (Projetil _ (Finita d1)) (Projetil _ (Finita d2)) = Finita (d1 + d2)

{-| Função que recebe um projetil incidente e a lista de projeteis ativos e retorna a lista de projeteis normalizada.

Isto é, normaliza de acordo com as sinergias e pela ordem de precedência referida no enunciado do projeto.

A lista de projetéis contém no máximo 2 elementos, as combinações possiveis na lista (se esta não for vazia) são:

* Fogo
* Gelo
* Resina
* Resina e Gelo

== Exemplos:

>>> atualizaProjeteis (Projetil Fogo (Finita 5)) [Projetil Resina (Finita 10)]
[Projetil Fogo (Finita 10.0)]

>>> atualizaProjeteis (Projetil Fogo (Finita 5)) [Projetil Resina (Finita 10), Projetil Gelo (Finita 3)]
[Projetil Resina (Finita 10.0)]
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

{-| Função que dada uma torre e o inimigo que será atingido pela torre retorna o inimigo
com a vida atualizada e com a lista de projetéis atualizada.

== Exemplos:

>>> atingeInimigo Torre {danoTorre = 10, projetilTorre = Projetil Fogo (Finita 5)} Inimigo {posicaoInimigo = (1,0), direcaoInimigo = Sul, vidaInimigo = 100, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}
Inimigo {posicaoInimigo = (1.0,0.0), direcaoInimigo = Sul, vidaInimigo = 90.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [Projetil {tipoProjetil = Fogo, duracaoProjetil = Finita 5.0}], tipoInimigo = Normal}

>>> atingeInimigo Torre {danoTorre = 20, projetilTorre = Projetil Gelo (Finita 3)} Inimigo {posicaoInimigo = (3,4), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 2.0, ataqueInimigo = 15, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Blindado}
Inimigo {posicaoInimigo = (3.0,4.0), direcaoInimigo = Este, vidaInimigo = 80.0, velocidadeInimigo = 2.0, ataqueInimigo = 15.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Blindado}
-}

atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo Torre {danoTorre = dano, projetilTorre = projetil} i@(Inimigo {tipoInimigo = Normal}) = 
    i {vidaInimigo = vidaInimigo i - dano, projeteisInimigo = atualizaProjeteis projetil (projeteisInimigo i)}
atingeInimigo Torre {danoTorre = dano} i@(Inimigo {tipoInimigo = Blindado}) =
    i {vidaInimigo = vidaInimigo i - dano, projeteisInimigo = []}

{-| Função que dado um portal e a lista de inimigos ativos no jogo
retorna uma tupla com o portal atualizado após a ativação do inimigo e a nova lista de inimigos ativos no jogo (com a adição do inimigo ativado).

== Exemplos:

>>> ativaInimigo Portal {posicaoPortal = (7.5,1.5), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 10, tempoOnda = 0, entradaOnda = 0}]} []
(Portal {posicaoPortal = (7.5,1.5), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 10, tempoOnda = 0, entradaOnda = 0}]},[])

>>> ativaInimigo Portal {posicaoPortal = (0.5,0.5), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Sul, vidaInimigo = 100, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}], cicloOnda = 5, tempoOnda = 0, entradaOnda = 0}]} []
(Portal {posicaoPortal = (0.5,0.5), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 5.0, tempoOnda = 5.0, entradaOnda = 0.0}]},[Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}])
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

{-| Função que dado um jogo verifica se o jogo terminou, i.e, se o jogador ganhou ou perdeu.

== Exemplos:

>>> terminouJogo Jogo {baseJogo = Base {posicaoBase = (0.5,0.5), vidaBase = 100}, portaisJogo = [Portal {posicaoPortal = (5.5,4.5), ondasPortal = []}], torresJogo = [], inimigosJogo = []}
True

>>> terminouJogo Jogo {baseJogo = Base {posicaoBase = (2.5,4.5), vidaBase = 100}, portaisJogo = [Portal {posicaoPortal = (7.5,3.5), ondasPortal = []}], torresJogo = [], inimigosJogo = [Inimigo {posicaoInimigo = (10.5,2.5), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}]}
False
-}

terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo (jogo) || perdeuJogo (jogo)

{-| Função que dado um jogo verifica se o jogador ganhou.

O jogador ganha quando a vida da base é positiva e não existam ondas nem inimigos ativos no jogo.

== Exemplos:

>>> ganhouJogo Jogo {baseJogo = Base {posicaoBase = (0.5,0.5), vidaBase = 100}, portaisJogo = [Portal {posicaoPortal = (5.5,4.5), ondasPortal = []}], torresJogo = [], inimigosJogo = []}
True
>>> ganhouJogo Jogo {baseJogo = Base {posicaoBase = (1.5,2.5), vidaBase = 80}, portaisJogo = [Portal {posicaoPortal = (2.5,1.5), ondasPortal = []}], torresJogo = [], inimigosJogo = [Inimigo {posicaoInimigo = (4.5,3.5), direcaoInimigo = Norte, vidaInimigo = 75.0, velocidadeInimigo = 2.5, ataqueInimigo = 15.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}]}
False
-}

ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = vida > 0 && null ondas && null inimigos
        where
            vida = vidaBase (baseJogo jogo)
            ondas = concatMap (ondasPortal) (portaisJogo (jogo))
            inimigos = inimigosJogo (jogo)

{-| Função que dado um jogo verifica se o jogador perdeu.

O jogador perde quando a vida da base deixa de ser positiva.

== Exemplos:

>>> perdeuJogo Jogo {baseJogo = Base {posicaoBase = (0.5,0.5), vidaBase = 0}, portaisJogo = [Portal {posicaoPortal = (6.5,2.5), ondasPortal = []}], torresJogo = [], inimigosJogo = []}
True

>>> perdeuJogo Jogo {baseJogo = Base {posicaoBase = (2.5,1.5), vidaBase = 65}, portaisJogo = [Portal {posicaoPortal = (4.5,0.5), ondasPortal = []}], torresJogo = [], inimigosJogo = [Inimigo {posicaoInimigo = (4.5,3.5), direcaoInimigo = Norte, vidaInimigo = 100.0, velocidadeInimigo = 1, ataqueInimigo = 15.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}]}
False
-}

perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vida <= 0
        where
            vida = vidaBase (baseJogo jogo)