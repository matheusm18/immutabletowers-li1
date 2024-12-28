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

-- | Função auxiliar que atualiza as ondas.
atualizaOnda :: Tempo -> Onda -> Onda
atualizaOnda t onda@(Onda {inimigosOnda = [], tempoOnda = trestantei, cicloOnda = tempoentreinimigos, entradaOnda = 0}) = onda
atualizaOnda t Onda {inimigosOnda = linimigos, tempoOnda = trestantei, cicloOnda = tempoentreinimigos, entradaOnda = 0} -- se onda estiver ativa:
    = case trestantei of
        0 -> Onda {inimigosOnda = tail linimigos, tempoOnda = tempoentreinimigos} -- falta botar o inimigo que saiu para jogo
        tr -> Onda {tempoOnda = max 0 (tr - t)}
atualizaOnda t Onda {inimigosOnda = linimigos, tempoOnda = trestantei, entradaOnda = tonda, cicloOnda = tempoentreinimigos}
    = Onda {entradaOnda = tnovoonda, inimigosOnda = linimigos, tempoOnda = trestantei, cicloOnda = tempoentreinimigos}
    where tnovoonda = max 0 (tonda - t)

-- | Função que atualiza todos os portais do jogo com o passar do tempo.
atualizaPortal :: Tempo -> Portal -> Portal
atualizaPortal t portal@(Portal {ondasPortal = [], posicaoPortal = pos}) = portal
atualizaPortal t portal =
    let (onda:restoondas) = ondasPortal portal
        ondaatualizada = atualizaOnda t onda
    in case (inimigosOnda ondaatualizada) of
         [] -> portal {ondasPortal = restoondas}
         _ -> portal {ondasPortal = ondaatualizada : restoondas}

-- | Função que atualiza todos os portais do jogo com o passar do tempo.
atualizaPortais :: Tempo -> [Portal] -> [Portal]
atualizaPortais t lportais = map (atualizaPortal t) lportais

-- | Função auxiliar que atualiza uma torre (cooldown).
atualizaTorre :: Tempo -> [Inimigo] -> Torre -> Torre
atualizaTorre t inimigos torre@(Torre {tempoTorre = 0})
    = if null (inimigosNoAlcance torre inimigos) 
      then torre
      else torre {tempoTorre = cicloTorre torre}
atualizaTorre t inimigos torre = torre {tempoTorre = max 0 (tempoTorre torre - t)}

-- | Função que atualiza todas as torres do jogo com o passar do tempo.
atualizaTorres :: Tempo -> [Inimigo] -> [Torre] -> [Torre]
atualizaTorres t inimigos torres = map (atualizaTorre t inimigos) torres

-- | Função que atualiza a vida de um inimigo de acordo com os projeteis ativos.
atualizaVidaProjeteis :: Inimigo -> Float
atualizaVidaProjeteis Inimigo {vidaInimigo = vida, projeteisInimigo = lprojeteis}
    = if (any (\proj -> tipoProjetil proj == Fogo) lprojeteis)
      then vida - 5 -- valor 5 é alterável (testar depois em jogo)
      else vida

moveInimigo :: Tempo -> Inimigo -> Mapa -> (Direcao,Posicao)
moveInimigo t Inimigo {posicaoInimigo = (x,y), direcaoInimigo = direcao, velocidadeInimigo = velocidade, projeteisInimigo = lprojeteis} mapa
    = if any (\proj -> tipoProjetil proj == Gelo) lprojeteis
      then (direcao,(x,y))
      else
        case direcao of
            Norte -> if validaPosicaoTerra (x,y+0.501) mapa
                     then (direcao,(x,y+(velocidade*t)))
                     else if validaPosicaoTerra (x+0.501,y) mapa
                     then (Este,(x+(velocidade*t),y))
                     else (Oeste,(x-velocidade*t,y))
  
            Sul ->  if validaPosicaoTerra (x,(y-0.501)) mapa
                    then (direcao,(x,(y-velocidade*t)))
                    else if validaPosicaoTerra (x+0.501,y) mapa
                    then (Este,(x+(velocidade*t),y))
                    else (Oeste,(x-velocidade*t,y))

            Este -> if validaPosicaoTerra (x+0.501,y) mapa
                    then (direcao,(x+(velocidade*t),y))
                    else if validaPosicaoTerra (x,y+0.501) mapa
                    then (Norte,(x,y+(velocidade*t)))
                    else (Sul,(x,y-velocidade*t))

            Oeste -> if validaPosicaoTerra (x-0.501,y) mapa
                     then (direcao,(x-(velocidade*t),y))
                     else if validaPosicaoTerra (x,y+0.501) mapa
                     then (Norte,(x,y+(velocidade*t)))
                     else (Sul,(x,y-velocidade*t))

-- | Função que atualiza a duração de um projétil com o passar do tempo.
atualizaProjetil :: Tempo -> Projetil -> Projetil
atualizaProjetil t proj@(Projetil {duracaoProjetil = Infinita}) = proj
atualizaProjetil t Projetil {duracaoProjetil = Finita d, tipoProjetil = tp} = Projetil {duracaoProjetil = Finita (d-t), tipoProjetil = tp}

-- | Função que dada uma lista de inimigos ativos retorna uma lista com os inimigos apos serem atacados por uma torre.
atacaInimigos :: Torre -> [Inimigo] -> [Inimigo]
atacaInimigos torre inimigos =
    let (iatacar,r) = splitAt (rajadaTorre torre) (inimigosNoAlcance torre inimigos)
        inimigosAtacados = map (atingeInimigo torre) iatacar
        inimigosRestantes = filter (`notElem` iatacar) inimigos
    in inimigosAtacados ++ inimigosRestantes

-- | Função auxiliar que atualiza os inimigos que já sofreram o dano da torre, isto é, movimentação, dano dos projéteis, etc.
atualizaInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
atualizaInimigo t mapa i
    = Inimigo {posicaoInimigo = posnova, 
               direcaoInimigo = direcaonova, 
               vidaInimigo = vidanova, 
               velocidadeInimigo = velocidadenova, 
               projeteisInimigo = lprojeteisnova,
               ataqueInimigo = ataqueInimigo i,
               butimInimigo = butimInimigo i}
    where lprojeteis = projeteisInimigo i
          velocidade = velocidadeInimigo i
          (direcaonova,posnova) = moveInimigo t i mapa
          vidanova = atualizaVidaProjeteis i
          lprojeteisnova = map (atualizaProjetil t) lprojeteis
          velocidadenova = if any (\proj -> tipoProjetil proj == Resina) lprojeteis then (velocidade * 0.5) else velocidade

-- | Função auxiliar que retorna o dano que os inimigos que chegaram a base causaram a ela.
getDanoNaBase :: [Inimigo] -> Base -> Float
getDanoNaBase inimigos base = danobase
    where inimigosbase = filter (\i -> posicaoInimigo i == posicaoBase base) inimigos
          danobase = sum (map (\i -> ataqueInimigo i) inimigosbase)

-- | Função auxiliar que retorna os créditos que os inimigos mortos deram a base.
getButim :: [Inimigo] -> Creditos
getButim inimigos = butim
    where inimigosmortos = filter (\i -> vidaInimigo i <= 0) inimigos
          butim = sum (map (\i -> butimInimigo i) inimigosmortos)

-- | Função que atualiza os inimigos do jogo (aplica dano, movimenta, etc) e no fim retorna uma tupla com a lista de inimigos atualizadas, o dano dos inimigos a base e o butim dos inimigos mortos.
atualizaInimigos :: Tempo -> Mapa -> Base -> [Torre] -> [Inimigo] -> ([Inimigo],Float,Creditos)
atualizaInimigos t mapa base torres inimigos 
    = let inimigosaposataque = concatMap (\torre -> if tempoTorre torre == 0 then atacaInimigos torre inimigos else inimigos) torres
          inimigosatualizados = map (atualizaInimigo t mapa) inimigosaposataque
          danobase = getDanoNaBase inimigosatualizados base
          butim = getButim inimigosatualizados
      in (filter (\i -> vidaInimigo i > 0 && posicaoInimigo i /= posicaoBase base) inimigosatualizados, danobase, butim)

-- | Função que atualiza a base, i.e, a vida da base e os créditos da base.
atualizaBase :: Tempo -> Base -> Float -> Creditos -> Base
atualizaBase _ base danobase butim
    = base {vidaBase = max 0 (vidaBase base - danobase), creditosBase = creditosBase base + butim}

-- | Função principal que atualiza o jogo com o passar do tempo.
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t jogo@(Jogo {baseJogo = base, portaisJogo = lportais, torresJogo = ltorres, inimigosJogo = linimigos, mapaJogo = mapa})
    =  let lportaisatt = atualizaPortais t lportais
           ltorresatt = atualizaTorres t linimigos ltorres
           (linimigosatt, danonabase, butim) = atualizaInimigos t mapa base ltorresatt linimigos
           baseatt = atualizaBase t base danonabase butim
        in jogo {baseJogo = baseatt, portaisJogo = lportaisatt, torresJogo = ltorresatt, inimigosJogo = linimigosatt}