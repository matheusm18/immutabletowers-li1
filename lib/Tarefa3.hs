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

-- | Função auxiliar que move o inimigo de acordo com a sua direção e velocidade.
moveInimigo :: Tempo -> Inimigo -> Mapa -> (Direcao,Posicao)
moveInimigo t Inimigo {posicaoInimigo = (x,y), direcaoInimigo = direcao, velocidadeInimigo = velocidade, projeteisInimigo = lprojeteis} mapa
    = if any (\proj -> tipoProjetil proj == Gelo) lprojeteis
      then (direcao,(x,y))
      else
        case direcao of
            Norte -> if validaPosicaoTerra (x,y-0.501) mapa
                     then (direcao,(x,y-(velocidade*t)))
                     else if validaPosicaoTerra (x+0.501,y) mapa
                     then (Este,(x,y))
                     else (Oeste,(x,y))

            Sul ->  if validaPosicaoTerra (x,(y+0.501)) mapa
                    then (direcao,(x,y+(velocidade*t)))
                    else if validaPosicaoTerra (x+0.501,y) mapa
                    then (Este,(x,y))
                    else (Oeste,(x,y))

            Este -> if validaPosicaoTerra (x+0.501,y) mapa
                    then (direcao,(x+(velocidade*t),y))
                    else if validaPosicaoTerra (x,y-0.501) mapa
                    then (Norte,(x,y))
                    else (Sul,(x,y))

            Oeste -> if validaPosicaoTerra (x-0.501,y) mapa
                     then (direcao,(x-(velocidade*t),y))
                     else if validaPosicaoTerra (x,y-0.501) mapa
                     then (Norte,(x,y))
                     else (Sul,(x,y))

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
atualizaInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
atualizaInimigo t mapa i
    = i {posicaoInimigo = posnova, 
         direcaoInimigo = direcaonova, 
         vidaInimigo = vidanova, 
         velocidadeInimigo = velocidadenova, 
         projeteisInimigo = lprojeteisnova}
    where lprojeteis = projeteisInimigo i
          velocidade = velocidadeInimigo i
          (direcaonova,posnova) = moveInimigo t i mapa
          vidanova = atualizaVidaProjeteis i
          velocidadenova = if any (\p -> tipoProjetil p == Resina) (lprojeteis) && not (any (\p -> tipoProjetil p == Resina) lprojeteisnova)
                           then velocidade * 2
                           else velocidade
          lprojeteisnova = filter (\p -> not (duracaoExpirou p)) (map (atualizaDurProjetil t) lprojeteis) -- ^ atualiza e remove os expirados

-- | Função que atualiza os inimigos do jogo (aplica dano, movimenta, etc) e no fim retorna uma tupla com a lista de inimigos atualizadas, o dano dos inimigos a base e o butim dos inimigos mortos.
atualizaInimigos :: Tempo -> Mapa -> Base -> [Inimigo] -> ([Inimigo], Float, Creditos)
atualizaInimigos t mapa base inimigos =
    let inimigosatualizados = map (atualizaInimigo t mapa) inimigos
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

{- A função 'atualizaJogo' é a função principal da Tarefa 3.

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