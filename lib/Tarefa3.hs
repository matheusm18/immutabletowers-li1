{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Matheus Henrique Monteiro da Silva Azevedo <a111430@alunos.uminho.pt>
              Francisco Luciano Martins <a111775@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425
import Tarefa1
import Tarefa2
import Data.Maybe (fromJust)

{-| Função que com o passar do tempo retorna o portal atualizado juntamente com a lista de inimigos do jogo incluindo os inimigos ativados.

== Exemplos:

>>> atualizaPortal 1 Portal {posicaoPortal = (1.5,2.5), ondasPortal = [Onda {cicloOnda = 10, tempoOnda = 10, entradaOnda = 5, inimigosOnda = [Inimigo {posicaoInimigo = (1.5,2.5), direcaoInimigo = Sul, vidaInimigo = 100, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}]}]} []
(Portal {posicaoPortal = (1.5,2.5), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1.5,2.5), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}], cicloOnda = 10.0, tempoOnda = 10.0, entradaOnda = 4.0}]}, [])

>>> atualizaPortal 1 Portal {posicaoPortal = (0.5,0.5), ondasPortal = [Onda {cicloOnda = 10, tempoOnda = 0, entradaOnda = 0, inimigosOnda = [Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Sul, vidaInimigo = 100, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}]}]} []
(Portal {posicaoPortal = (0.5,0.5), ondasPortal = [Onda {inimigosOnda = [], cicloOnda = 10.0, tempoOnda = 10.0, entradaOnda = 0.0}]}, [Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}])
-}

atualizaPortal :: Tempo -> Portal -> [Inimigo] -> (Portal, [Inimigo])
atualizaPortal _ portal@(Portal {ondasPortal = []}) inimigos = (portal,inimigos)
atualizaPortal t portal inimigos =
    let (onda:restoondas) = ondasPortal portal
    in  if inimigosOnda onda == []
        then (portal {ondasPortal = restoondas}, inimigos)
        else if entradaOnda onda == 0 && tempoOnda onda == 0
        then (ativaInimigo portal inimigos)
        else if entradaOnda onda == 0 && tempoOnda onda > 0
        then (portal {ondasPortal = (onda { tempoOnda = max 0 (tempoOnda onda - t) }) : restoondas}, inimigos)
        else (portal {ondasPortal = (onda {entradaOnda = max 0 (entradaOnda onda - t)}) : restoondas}, inimigos)

{-| Função que atualiza todos os portais do jogo com o passar do tempo.

== Exemplos:

>>> atualizaPortais 2 [] []
([], [])

>>> atualizaPortais 1 [Portal {posicaoPortal = (1.5,3.5), ondasPortal = [Onda {cicloOnda = 10, tempoOnda = 10, entradaOnda = 5, inimigosOnda = [Inimigo {posicaoInimigo = (1.5,3.5), direcaoInimigo = Sul, vidaInimigo = 100, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}]}]}] []
([Portal {posicaoPortal = (1.5,3.5), ondasPortal = [Onda {inimigosOnda = [Inimigo {posicaoInimigo = (1.5,3.5), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}], cicloOnda = 10.0, tempoOnda = 10.0, entradaOnda = 4.0}]}], [])
-}

atualizaPortais :: Tempo -> [Portal] -> [Inimigo] -> ([Portal],[Inimigo])
atualizaPortais _ [] inimigos = ([],inimigos)
atualizaPortais t (p1:rps) inimigos = let (p1att, inimigosp1att) = atualizaPortal t p1 inimigos
                                          (portais, inimigosatualizados) = atualizaPortais t rps inimigosp1att
                                      in ((p1att : portais), inimigosatualizados)

-- | Função que dada uma torre e uma lista de inimigos ativos retorna uma lista com os inimigos apos serem atacados por esta torre e retorna a torre com o cooldown renovado, se ela atacar.
atacaInimigos :: Torre -> [Inimigo] -> (Torre,[Inimigo])
atacaInimigos torre inimigos =
    let (iatacar,_) = splitAt (rajadaTorre torre) (inimigosNoAlcance torre inimigos)
        inimigosAtacados = map (atingeInimigo torre) iatacar
        inimigosRestantes = filter (`notElem` iatacar) inimigos
    in  if null iatacar then (torre, inimigos)
        else (torre {tempoTorre = cicloTorre torre}, inimigosAtacados ++ inimigosRestantes)

-- | Função que atualiza as torres: ataca os inimigos e retorna a tupla com a torre atualizada e a lista dos inimigos após o ataque.
atualizaTorres :: Tempo -> [Torre] -> [Inimigo] -> ([Torre], [Inimigo])
atualizaTorres t torres inimigos = foldl atualizaAux ([], inimigos) torres
  where
    atualizaAux :: ([Torre], [Inimigo]) -> Torre -> ([Torre], [Inimigo])
    atualizaAux (acctorres, accinimigos) torre
        | tempoTorre torre == 0 = let (torreAtt, inimigosAposAtaque) = atacaInimigos torre accinimigos
                                  in (torreAtt : acctorres, inimigosAposAtaque)
        | otherwise = let torreAtualizada = torre { tempoTorre = max 0 (tempoTorre torre - t) }
                      in (torreAtualizada : acctorres, accinimigos)                          

{-| Função que atualiza a vida de um inimigo de acordo com os projeteis ativos.

== Exemplos:

>>> atualizaVidaProjeteis (Inimigo {vidaInimigo = 100, projeteisInimigo = [Projetil {duracaoProjetil = Finita 5, tipoProjetil = Fogo}]})
100 - (1/30)

>>> atualizaVidaProjeteis (Inimigo {vidaInimigo = 75, projeteisInimigo = [Projetil {duracaoProjetil = Finita 3, tipoProjetil = Gelo}]})
75
-}

atualizaVidaProjeteis :: Inimigo -> Float
atualizaVidaProjeteis Inimigo {vidaInimigo = vida, projeteisInimigo = lprojeteis}
    = if (any (\proj -> tipoProjetil proj == Fogo) lprojeteis)
      then vida - (1/30) -- (1/30) para dar 2 de dano por segundo com o fps a 60 (depois temos que testar em jogo)
      else vida

{-| Função para dar floor nas componentes x e y das posições para obter a "posição geral".

Como sabemos, na matriz toda posição em que o x pertence a [0,1] e o y pertence a [0,1] é a posição (0,0) da matriz, essa função acha essa posição "geral" da matriz.

== Exemplos:

>>> floorPosicao (0.5,0.5)
(0.0,0.0)

>>> floorPosicao (1.5,2.5)
(1.0,2.0)
-}

floorPosicao :: Posicao -> Posicao
floorPosicao (x, y) = (fromIntegral (floor x),fromIntegral (floor y))

{-| Função que encontra o caminho mais curto entre as duas posições (semelhante a função da Tarefa 1).

Para essa função funcionar corretamente precisamos dar as posições do centro do quadrado da posição inicial e final.

No fim, é devolvido o caminho com as posições gerais (para facilitar posteriormente na moveInimigo).

== Exemplos:

>>> encontrarCaminho (0,0) (1,1) [[Terra, Terra, Agua],[Agua,Terra,Agua],[Relva,Agua,Agua]]
Just [(0.0,0.0),(1.0,0.0),(1.0,1.0)]

>>> encontrarCaminho (0.5,0.5) (1.5,2.5) [[Terra, Terra, Agua],[Agua,Terra,Agua],[Relva,Terra,Agua]]
Just [(0.0,0.0),(1.0,0.0),(1.0,1.0),(1.0,2.0)]

>>> encontrarCaminho (0.5,0.5) (1.5,1.5) [[Terra, Agua],[Agua,Terra]]
Nothing
-}

encontrarCaminho :: Posicao -> Posicao -> Mapa -> Maybe [Posicao]
encontrarCaminho posinicial posfinal mapa = encontrarCaminhoAux [[posinicial]] []
  where
    encontrarCaminhoAux :: [[Posicao]] -> [Posicao] -> Maybe [Posicao]
    encontrarCaminhoAux [] _ = Nothing
    encontrarCaminhoAux (caminho:fila) visitados
        | floorPosicao atual == floorPosicao posfinal = Just ((map) (floorPosicao) caminho) -- ^ verificar se tá na mesma posição geral do quadrado da posição final
        | atual `elem` visitados = encontrarCaminhoAux fila visitados
        | otherwise =
            let visitados' = atual : visitados
                adjacentes = filter (`notElem` visitados') (posAdjacentes atual mapa) -- ^ pos adjacentes são de terra
                novosCaminhos = [caminho ++ [adj] | adj <- adjacentes] -- ^ lista de caminhos (cada caminho é uma lista de posições)
            in (encontrarCaminhoAux (fila ++ novosCaminhos) visitados')
        where atual = last caminho -- ^ a pos final de um caminho é a que interessa para comparar 

{-| Função auxiliar que retorna as posições válidas para o inimigo se mover (nunca voltar para trás).

== Exemplos:

>>> getPosicoesValidas (1.5,1.5) Este [[Terra, Terra, Agua],[Terra,Terra,Agua],[Relva,Terra,Agua]]
[((1.5,0.999),Norte),((1.5,2.001),Sul)]

>>> getPosicoesValidas (2.5,1.5) Este [[Terra, Agua, Agua,Agua],[Terra,Terra,Terra,Terra],[Relva,Relva,Relva,Terra],[Relva,Terra,Terra,Terra]]
[((3.001,1.5),Este)]
-}

getPosicoesValidas :: Posicao -> Direcao -> Mapa -> [(Posicao,Direcao)]
getPosicoesValidas (x,y) dir mapa = 
    case dir of
        Norte -> filter (\((x,y),_) -> validaPosicaoTerra (x,y) mapa) [((x,y-0.501),Norte),((x+0.501,y),Este),((x-0.501,y),Oeste)]
        Sul -> filter (\((x,y),_) -> validaPosicaoTerra (x,y) mapa) [((x,y+0.501),Sul),((x+0.501,y),Este),((x-0.501,y),Oeste)]
        Este -> filter (\((x,y),_) -> validaPosicaoTerra (x,y) mapa) [((x+0.501,y),Este),((x,y-0.501),Norte),((x,y+0.501),Sul)]
        Oeste -> filter (\((x,y),_) -> validaPosicaoTerra (x,y) mapa) [((x-0.501,y),Oeste),((x,y-0.501),Norte),((x,y+0.501),Sul)]

{-| Função auxiliar que escolhe a direção que o inimigo deve seguir para chegar a base quando há mais de uma direção possível.

A primeira lista é a lista das tuplas de posições e direções válidas para o inimigo se mover.
A segunda lista é a lista de posições do caminho que o inimigo deve seguir.

== Exemplos:

>>> escolheDirecao [((1.001,0.5),Este)] [(1,0),(1,1)]
Just Este

>>> escolheDirecao [((2.001,0.5),Norte),((2.5,1.001),Sul)] [(2,1),(2,2)]
Just Sul
-}

escolheDirecao :: [(Posicao,Direcao)] -> [Posicao] -> Maybe Direcao
escolheDirecao [] _ = Nothing
escolheDirecao _ [] = Nothing
escolheDirecao ((pos,d):rl) caminho
    | elem (floorPosicao pos) caminho = Just d
    | otherwise = escolheDirecao rl caminho

{-| Função auxiliar que verifica se uma dada posição pertence ao caminho que o inimigo deve seguir.

== Exemplos:

>>> pertenceCaminho (1.5,1.5) [(1,1),(1,2),(1,3),(2,3),(3,3)]
True

>>> pertenceCaminho (0.5,0.5) [(1,1),(1,2),(1,3),(2,3),(3,3)]
False
-}

pertenceCaminho :: Posicao -> [Posicao] -> Bool
pertenceCaminho _ [] = False
pertenceCaminho (x,y) caminho = elem (x',y') caminho
    where (x',y') = floorPosicao (x,y) -- ^ arredonda para pegar a posição "toda" (a da matriz)
 
{-| Função auxiliar que retorna a posição do centro do quadrado para evitar bug de movimento.

Sem essa função, antes na moveInimigo, quando o inimigo não podia continuar na mesma direção e tinha de fazer uma tomada de decisão,
aconteciam casos em que o inimigo havia passado do centro do quadrado, por ex: estava em (9.55,4.55), 
o inimigo deveria ir pra Sul mas a função getPosicoesValida retornava
tanto a parte do Norte desse mesmo quadrado quanto a posicao imediatamente em baixo, o que causava bug.

Ao chamar a função getPosCentroQuadrado para a posicao (x,y) que queremos usar na getPosicoesValidas, a moveInimigo funciona.

== Exemplos:

>>> getPosCentroQuadrado (9.0,4.75)
(9.5,4.5)

>>> getPosCentroQuadrado (0.0,1.0)
(0.5,1.5)
-}

getPosCentroQuadrado :: Posicao -> Posicao
getPosCentroQuadrado (x,y) = (fromIntegral (floor x) + 0.5, fromIntegral (floor y) + 0.5)

{-| Função que move o inimigo de acordo com a sua direção e velocidade.

== Exemplos:

>>> moveInimigo 1 Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Este, velocidadeInimigo = 1.0, projeteisInimigo = [], tipoInimigo = Normal} (2.5,2.5) [[Terra, Terra, Agua],[Relva,Terra,Agua],[Relva,Terra,Terra]]
(Este,(1.5,0.5))

>>> moveInimigo 1 Inimigo {posicaoInimigo = (1.5,0.5), direcaoInimigo = Este, velocidadeInimigo = 1.0, projeteisInimigo = [], tipoInimigo = Normal} (2.5,2.5) [[Terra, Terra, Agua],[Relva,Terra,Agua],[Relva,Terra,Terra]]
(Sul,(1.5,0.5))

>>> moveInimigo 1 Inimigo {posicaoInimigo = (1.5,0.5), direcaoInimigo = Sul, velocidadeInimigo = 1.0, projeteisInimigo = [Projetil Gelo (Finita 3)], tipoInimigo = Normal} (2.5,2.5) [[Terra, Terra, Agua],[Relva,Terra,Agua],[Relva,Terra,Terra]]
(Sul,(1.5,0.5))
-}

moveInimigo :: Tempo -> Inimigo -> Posicao -> Mapa -> (Direcao,Posicao)
moveInimigo t Inimigo {posicaoInimigo = (x,y), direcaoInimigo = direcao, velocidadeInimigo = velocidade, projeteisInimigo = lprojeteis} posbase mapa
    = if any (\proj -> tipoProjetil proj == Gelo) lprojeteis
      then (direcao,(x,y))
      else
        let fatorReducao = 0.5 -- ^ fator de reduçãoo de velocidade para a resina
            velocidadeAtual = if any (\proj -> tipoProjetil proj == Resina) lprojeteis then velocidade*fatorReducao else velocidade
            caminho = (fromJust(encontrarCaminho (getPosCentroQuadrado (x,y)) (posbase) mapa))
            posicoesValidas = getPosicoesValidas (getPosCentroQuadrado (x,y)) direcao mapa -- ^ com a posicaoCentroQuadrado não buga mais o movimento
        in case direcao of
            Norte -> if pertenceCaminho (x,y-0.501) caminho
                     then (direcao,(x,y-(velocidadeAtual*t)))
                     else (fromJust (escolheDirecao posicoesValidas caminho), (x,y)) -- ^ tomada de decisão (mais de uma direção possível e a direção Norte não leva a base)

            Sul ->  if pertenceCaminho (x,(y+0.501)) caminho
                    then (direcao,(x,y+(velocidadeAtual*t)))
                    else (fromJust (escolheDirecao posicoesValidas caminho), (x,y))

            Este -> if pertenceCaminho (x+0.501,y) caminho
                    then (direcao,(x+(velocidadeAtual*t),y))
                    else (fromJust (escolheDirecao posicoesValidas caminho), (x,y))

            Oeste -> if pertenceCaminho (x-0.501,y) caminho
                     then (direcao,(x-(velocidadeAtual*t),y))
                     else (fromJust (escolheDirecao posicoesValidas caminho), (x,y))

{-| Função que atualiza a duração de um projétil com o passar do tempo.

== Exemplos:

>>> atualizaDurProjetil 3 (Projetil Fogo (Finita 5))
Projetil Fogo (Finita 2.0)

>>> atualizaDurProjetil 1 (Projetil Gelo Infinita)
Projetil Gelo Infinita
-}

atualizaDurProjetil :: Tempo -> Projetil -> Projetil
atualizaDurProjetil _ proj@(Projetil {duracaoProjetil = Infinita}) = proj
atualizaDurProjetil t Projetil {duracaoProjetil = Finita d, tipoProjetil = tp} 
    = if (d-t) >= 0 then Projetil {duracaoProjetil = Finita (d-t), tipoProjetil = tp}
      else Projetil {duracaoProjetil = Finita 0, tipoProjetil = tp}

{-| Função que verifica se a duração de um projétil expirou.

== Exemplos:

>>> duracaoExpirou (Projetil Fogo (Finita 0))
True

>>> duracaoExpirou (Projetil Gelo (Finita 5))
False
-}

duracaoExpirou :: Projetil -> Bool
duracaoExpirou Projetil {duracaoProjetil = Finita 0} = True
duracaoExpirou _ = False

{-| Função auxiliar que atualiza os inimigos que já sofreram o dano da torre, isto é, movimentação, dano dos projéteis, etc.

== Exemplos:

>>> atualizaInimigo 1 [[Terra, Terra, Agua],[Relva,Terra,Agua],[Relva,Terra,Terra]] (2.5,2.5) Inimigo {posicaoInimigo = (1.5,1.5), direcaoInimigo = Sul, vidaInimigo = 100, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}
Inimigo {posicaoInimigo = (1.5,2.5), direcaoInimigo = Sul, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}

>>> atualizaInimigo 1 [[Terra, Terra, Agua],[Relva,Terra,Agua],[Relva,Agua,Agua]] (1.5,1.5) Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [Projetil Gelo (Finita 3)], tipoInimigo = Normal}
Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Este, vidaInimigo = 100.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [Projetil {tipoProjetil = Gelo, duracaoProjetil = Finita 2.0}], tipoInimigo = Normal}
-}

atualizaInimigo :: Tempo -> Mapa -> Posicao -> Inimigo -> Inimigo
atualizaInimigo t mapa posbase i
    = i {posicaoInimigo = posnova, 
         direcaoInimigo = direcaonova, 
         vidaInimigo = vidanova,
         projeteisInimigo = lprojeteisnova}
    where lprojeteis = projeteisInimigo i
          (direcaonova,posnova) = moveInimigo t i posbase mapa
          vidanova = atualizaVidaProjeteis i
          lprojeteisnova = filter (\p -> not (duracaoExpirou p)) (map (atualizaDurProjetil t) lprojeteis) -- ^ atualiza e remove os expirados

{-| Função que atualiza os inimigos do jogo (aplica dano, movimenta, etc) e no fim retorna uma tupla 
com a lista de inimigos atualizadas, o dano dos inimigos a base e o butim dos inimigos mortos.

== Exemplos:

>>> atualizaInimigos 1 [[Terra, Terra, Agua],[Relva,Terra,Agua],[Relva,Terra,Terra]] Base {vidaBase = 100, posicaoBase = (2.5,2.5), creditosBase = 50} [Inimigo {posicaoInimigo = (1.5,2.5), direcaoInimigo = Este, vidaInimigo = 100, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Normal}]
([],10.0,0)

>>> atualizaInimigo 1 [[Terra, Terra, Agua],[Relva,Terra,Agua],[Relva,Agua,Agua]] (1.5,1.5) Inimigo {posicaoInimigo = (0.5,0.5), direcaoInimigo = Este, vidaInimigo = 75, velocidadeInimigo = 1.0, ataqueInimigo = 10, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Blindado}
Inimigo {posicaoInimigo = (1.5,0.5), direcaoInimigo = Este, vidaInimigo = 75.0, velocidadeInimigo = 1.0, ataqueInimigo = 10.0, butimInimigo = 25, projeteisInimigo = [], tipoInimigo = Blindado}
-}

atualizaInimigos :: Tempo -> Mapa -> Base -> [Inimigo] -> ([Inimigo], Float, Creditos)
atualizaInimigos t mapa base inimigos =
    let inimigosatualizados = map (atualizaInimigo t mapa (posicaoBase base)) inimigos
        danobase = getDanoNaBase inimigosatualizados base
        butim = getButim inimigosatualizados
    in (filter (\i -> vidaInimigo i > 0 && dist(posicaoInimigo i) (posicaoBase base) > 0.35) inimigosatualizados, danobase, butim)

{-| Função auxiliar que retorna o dano que os inimigos que chegaram a base causaram a ela.

== Exemplos:

>>> getDanoNaBase [Inimigo {posicaoInimigo = (0.5,0.5), vidaInimigo = 100, ataqueInimigo = 10}] (Base {vidaBase = 250, posicaoBase = (0.5,0.5), creditosBase = 150})
10.0

>>> getDanoNaBase [] (Base {vidaBase = 100, posicaoBase = (7.5,5.5), creditosBase = 150})
0.0
-}

getDanoNaBase :: [Inimigo] -> Base -> Float
getDanoNaBase inimigos base = danobase
    where inimigosbase = filter (\i -> dist (posicaoInimigo i) (posicaoBase base) <= 0.35) inimigos -- ^ comparar as posições estava a dar erro, então optamos por <= 0.35
          danobase = sum (map (\i -> ataqueInimigo i) inimigosbase)

{-| Função auxiliar que retorna os créditos que os inimigos mortos deram a base.

== Exemplos:

>>> getButim [Inimigo {vidaInimigo = 0, butimInimigo = 50}, Inimigo {vidaInimigo = 0, butimInimigo = 100}]
150

>>> getButim [Inimigo {vidaInimigo = 100, butimInimigo = 25}]
0
-}

getButim :: [Inimigo] -> Creditos
getButim inimigos = butim
    where inimigosmortos = filter (\i -> vidaInimigo i <= 0) inimigos
          butim = sum (map (\i -> butimInimigo i) inimigosmortos)

{-| Função que atualiza a base, i.e, a vida da base e os créditos da base.

== Exemplos:
>>> atualizaBase 1 (Base {vidaBase = 250, posicaoBase = (5.5,4.5), creditosBase = 150}) 20 50
Base {vidaBase = 230.0, posicaoBase = (5.5,4.5), creditosBase = 200}

>>> atualizaBase 1 (Base {vidaBase = 250, posicaoBase = (0.5,9.5), creditosBase = 100}) 0 0
Base {vidaBase = 250.0, posicaoBase = (0.5,9.5), creditosBase = 100}
-}

atualizaBase :: Tempo -> Base -> Float -> Creditos -> Base
atualizaBase _ base danobase butim
    = base {vidaBase = max 0 (vidaBase base - danobase), creditosBase = creditosBase base + butim}

{-| A função 'atualizaJogo' é a função principal da Tarefa 3.

Esta função atualiza o jogo com base no tempo que passou desde a última atualização.
De modo a retornar a base, os portais, as torres e os inimigos atualizados.
-}

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t jogo@(Jogo {baseJogo = base, portaisJogo = lportais, torresJogo = ltorres, inimigosJogo = linimigos, mapaJogo = mapa})
    =  let (ltorresatt, linimigosaposataque) = atualizaTorres t ltorres linimigos -- ^ atualiza as torres e os inimigos apos o ataque
           (linimigosatt, danonabase, butim) = atualizaInimigos t mapa base linimigosaposataque -- ^ atualiza os inimigos (após terem sido atacados)
           baseatt = atualizaBase t base danonabase butim -- ^ atualiza a base (vida e créditos)
           (portaisapossaida,inimigosattcativados) = atualizaPortais t lportais linimigosatt -- ^ atualiza os portais e ativa os inimigos
       in jogo {baseJogo = baseatt, portaisJogo = portaisapossaida, torresJogo = ltorresatt, inimigosJogo = inimigosattcativados}