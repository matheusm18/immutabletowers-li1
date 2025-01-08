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

-- | Função que com o passar do tempo retorna o portal atualizado juntamente com a lista de inimigos do jogo incluindo os inimigos ativados.
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

-- | Função que atualiza todos os portais do jogo com o passar do tempo.
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

-- | Função que atualiza a vida de um inimigo de acordo com os projeteis ativos.
atualizaVidaProjeteis :: Inimigo -> Float
atualizaVidaProjeteis Inimigo {vidaInimigo = vida, projeteisInimigo = lprojeteis}
    = if (any (\proj -> tipoProjetil proj == Fogo) lprojeteis)
      then vida - (1/30) -- (1/30) para dar 2 de dano por segundo com o fps a 60 (depois temos que testar em jogo)
      else vida

{-| Função para arredondar as posições para obter a posição geral

Como sabemos, na matriz toda posição em que o x pertence a [0,1] e o y pertence a [0,1] é a posição (0,0) da matriz, essa função acha essa posição "geral" da matriz.
-}

arredondarPosicao :: Posicao -> (Int, Int)
arredondarPosicao (x, y) = (floor x,floor y)

-- | Função que encontra o caminho mais curto entre as duas posições (semelhante a função da Tarefa 1)
encontrarCaminho :: Posicao -> Posicao -> Mapa -> Maybe [Posicao]
encontrarCaminho posinicial posfinal mapa = encontrarCaminhoAux [[posinicial]] []
  where
    encontrarCaminhoAux :: [[Posicao]] -> [Posicao] -> Maybe [Posicao]
    encontrarCaminhoAux [] _ = Nothing
    encontrarCaminhoAux (caminho:fila) visitados
        | arredondarPosicao atual == arredondarPosicao posfinal = Just caminho -- ^ verificar se tá na mesma posição geral do quadrado da posição final
        | atual `elem` visitados = encontrarCaminhoAux fila visitados
        | otherwise =
            let visitados' = atual : visitados
                adjacentes = filter (`notElem` visitados') (posAdjacentes atual mapa) -- ^ pos adjacentes são de terra
                novosCaminhos = [caminho ++ [adj] | adj <- adjacentes] -- ^ lista de caminhos (cada caminho é uma lista de posições)
            in (encontrarCaminhoAux (fila ++ novosCaminhos) visitados')
        where atual = last caminho -- ^ a pos final de um caminho é a que interessa para comparar 

-- | Função auxiliar que retorna as posições válidas para o inimigo se mover (nunca voltar para trás).
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

-}

escolheDirecao :: [(Posicao,Direcao)] -> [Posicao] -> Maybe Direcao
escolheDirecao [] _ = Nothing
escolheDirecao _ [] = Nothing
escolheDirecao ((pos,d):rl) caminho
    | elem (posicaoArredondada pos) caminho = Just d
    | otherwise = escolheDirecao rl caminho
    where posicaoArredondada :: Posicao -> Posicao
          posicaoArredondada (x,y) = (fromIntegral (floor x),fromIntegral (floor y))

-- | Função auxiliar que verifica se uma dada posição pertence ao caminho que o inimigo deve seguir.
pertenceCaminho :: Posicao -> [Posicao] -> Bool
pertenceCaminho _ [] = False
pertenceCaminho (x,y) caminho = elem (x',y') caminho
    where (x',y') = (fromIntegral (floor x),fromIntegral (floor y)) -- ^ arredonda para pegar a posição "toda" (a da matriz)
 
-- | Função que move o inimigo de acordo com a sua direção e velocidade.
moveInimigo :: Tempo -> Inimigo -> Posicao -> Mapa -> (Direcao,Posicao)
moveInimigo t Inimigo {posicaoInimigo = (x,y), direcaoInimigo = direcao, velocidadeInimigo = velocidade, projeteisInimigo = lprojeteis} posbase mapa
    = if any (\proj -> tipoProjetil proj == Gelo) lprojeteis
      then (direcao,(x,y))
      else
        let caminho = map (\(x,y) -> (fromIntegral (floor x),fromIntegral(floor y))) (fromJust(encontrarCaminho (x,y) (posbase) mapa)) -- ^ passa o caminho para as posições "gerais"
            posicoesvalidas = getPosicoesValidas (x,y) direcao mapa
        in case direcao of
            Norte -> if pertenceCaminho (x,y-0.501) caminho
                     then (direcao,(x,y-(velocidade*t)))
                     else (fromJust (escolheDirecao posicoesvalidas caminho), (x,y)) -- ^ tomada de decisão (mais de uma direção possível e a direção Norte não leva a base)

            Sul ->  if pertenceCaminho (x,(y+0.501)) caminho
                    then (direcao,(x,y+(velocidade*t)))
                    else (fromJust (escolheDirecao posicoesvalidas caminho), (x,y))

            Este -> if pertenceCaminho (x+0.501,y) caminho
                    then (direcao,(x+(velocidade*t),y))
                    else (fromJust (escolheDirecao posicoesvalidas caminho), (x,y))

            Oeste -> if pertenceCaminho (x-0.501,y) caminho
                     then (direcao,(x-(velocidade*t),y))
                     else (fromJust (escolheDirecao posicoesvalidas caminho), (x,y))

-- | Função que atualiza a duração de um projétil com o passar do tempo.
atualizaDurProjetil :: Tempo -> Projetil -> Projetil
atualizaDurProjetil _ proj@(Projetil {duracaoProjetil = Infinita}) = proj
atualizaDurProjetil t Projetil {duracaoProjetil = Finita d, tipoProjetil = tp} 
    = if (d-t) >= 0 then Projetil {duracaoProjetil = Finita (d-t), tipoProjetil = tp}
      else Projetil {duracaoProjetil = Finita 0, tipoProjetil = tp}

-- | Função que verifica se a duração de um projétil expirou
duracaoExpirou :: Projetil -> Bool
duracaoExpirou Projetil {duracaoProjetil = Finita 0} = True
duracaoExpirou _ = False

-- | Função auxiliar que atualiza os inimigos que já sofreram o dano da torre, isto é, movimentação, dano dos projéteis, etc.
atualizaInimigo :: Tempo -> Mapa -> Posicao -> Inimigo -> Inimigo
atualizaInimigo t mapa posbase i
    = i {posicaoInimigo = posnova, 
         direcaoInimigo = direcaonova, 
         vidaInimigo = vidanova, 
         velocidadeInimigo = velocidadenova, 
         projeteisInimigo = lprojeteisnova}
    where lprojeteis = projeteisInimigo i
          velocidade = velocidadeInimigo i
          (direcaonova,posnova) = moveInimigo t i posbase mapa
          vidanova = atualizaVidaProjeteis i
          velocidadenova = if any (\p -> tipoProjetil p == Resina) (lprojeteis) && not (any (\p -> tipoProjetil p == Resina) lprojeteisnova)
                           then velocidade * 2
                           else velocidade
          lprojeteisnova = filter (\p -> not (duracaoExpirou p)) (map (atualizaDurProjetil t) lprojeteis) -- ^ atualiza e remove os expirados

-- | Função que atualiza os inimigos do jogo (aplica dano, movimenta, etc) e no fim retorna uma tupla com a lista de inimigos atualizadas, o dano dos inimigos a base e o butim dos inimigos mortos.
atualizaInimigos :: Tempo -> Mapa -> Base -> [Inimigo] -> ([Inimigo], Float, Creditos)
atualizaInimigos t mapa base inimigos =
    let inimigosatualizados = map (atualizaInimigo t mapa (posicaoBase base)) inimigos
        danobase = getDanoNaBase inimigosatualizados base
        butim = getButim inimigosatualizados
    in (filter (\i -> vidaInimigo i > 0 && dist(posicaoInimigo i) (posicaoBase base) > 0.35) inimigosatualizados, danobase, butim)

-- | Função auxiliar que retorna o dano que os inimigos que chegaram a base causaram a ela.
getDanoNaBase :: [Inimigo] -> Base -> Float
getDanoNaBase inimigos base = danobase
    where inimigosbase = filter (\i -> dist (posicaoInimigo i) (posicaoBase base) <= 0.35) inimigos -- ^ comparar as posições estava a dar erro, então optamos por <= 0.35
          danobase = sum (map (\i -> ataqueInimigo i) inimigosbase)

-- | Função auxiliar que retorna os créditos que os inimigos mortos deram a base.
getButim :: [Inimigo] -> Creditos
getButim inimigos = butim
    where inimigosmortos = filter (\i -> vidaInimigo i <= 0) inimigos
          butim = sum (map (\i -> butimInimigo i) inimigosmortos)

-- | Função que atualiza a base, i.e, a vida da base e os créditos da base.
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