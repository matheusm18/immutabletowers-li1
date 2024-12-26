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
atualizaOnda t Onda {inimigosOnda = linimigos, tempoOnda = trestantei, cicloOnda = tempoentreinimigos, entradaOnda = 0} -- se onda estiver ativa:
    = case trestantei of
        0 -> Onda {inimigosOnda = tail linimigos, tempoOnda = tempoentreinimigos} -- falta botar o inimigo que saiu para jogo
        x -> Onda {tempoOnda = max 0 (x - t)}
atualizaOnda t Onda {inimigosOnda = linimigos, tempoOnda = trestantei, entradaOnda = tonda}
    = Onda {entradaOnda = tnovoonda}
    where tnovoonda = max 0 (tonda - t)

-- | Função que atualiza todos os portais do jogo com o passar do tempo.
atualizaPortal :: Tempo -> Portal -> Portal
atualizaPortal t Portal {ondasPortal = londas} =
    case (inimigosOnda onda) of
         [] -> Portal {ondasPortal = restoondas}
         _ -> Portal {ondasPortal = ondaatualizada:restoondas}
    where
        (onda:restoondas) = londas
        ondaatualizada = atualizaOnda t onda

-- | Função que atualiza todos os portais do jogo com o passar do tempo.
atualizaPortais :: Tempo -> [Portal] -> [Portal]
atualizaPortais t lportais = map (atualizaPortal t) lportais

-- | Função auxiliar que atualiza uma torre (cooldown).
atualizaTorre :: Tempo -> [Inimigo] -> Torre -> Torre
atualizaTorre t inimigos torre@(Torre {tempoTorre = 0, cicloTorre = tempo})
    = if null (inimigosNoAlcance torre inimigos) then Torre {tempoTorre = 0} else Torre {tempoTorre = tempo}
atualizaTorre t inimigos Torre {tempoTorre = temporestante} = let temponovo = max 0 (temporestante - t)
                                                     in  Torre {tempoTorre = temponovo}

-- | Função que atualiza todas as torres do jogo com o passar do tempo.
atualizaTorres :: Tempo -> [Inimigo] -> [Torre] -> [Torre]
atualizaTorres t inimigos torres = map (atualizaTorre t inimigos) torres

-- | Função que atualiza a vida de um inimigo de acordo com os projeteis ativos.
atualizaVidaProjeteis :: Inimigo -> Float
atualizaVidaProjeteis Inimigo {vidaInimigo = vida, projeteisInimigo = lprojeteis}
    = if (any (\proj -> tipoProjetil proj == Fogo) lprojeteis)
      then vida - 5 -- valor 5 é alterável (testar depois em jogo)
      else vida

-- | Função que move os inimigos ** ver melhor se a movimentação está correta **
moveInimigo :: Tempo -> Inimigo -> Mapa -> (Direcao,Posicao)
moveInimigo t Inimigo {posicaoInimigo = (x,y), direcaoInimigo = direcao, velocidadeInimigo = velocidade, projeteisInimigo = lprojeteis} mapa
    = if any (\proj -> tipoProjetil proj == Gelo) lprojeteis
      then (direcao,(x,y))
      else
        case direcao of
            Norte -> if validaPosicaoTerra (x,y+(velocidade*t)) mapa
                     then (direcao,(x,y+(velocidade*t)))
                     else if validaPosicaoTerra (x+(velocidade*t),y) mapa
                     then (Este,(x,y))
                     else (Oeste,(x,y))
            Sul ->  if validaPosicaoTerra (x,(y-velocidade*t)) mapa
                    then (direcao,(x,(y-velocidade*t)))
                    else if validaPosicaoTerra (x+(velocidade*t),y) mapa
                    then (Este,(x,y))
                    else (Oeste,(x,y))
            Este -> if validaPosicaoTerra (x+(velocidade*t),y) mapa
                    then (direcao,(x+(velocidade*t),y))
                    else if validaPosicaoTerra (x,y+(velocidade*t)) mapa
                    then (Norte,(x,y))
                    else (Sul,(x,y))
            Oeste -> if validaPosicaoTerra (x-(velocidade*t),y) mapa
                     then (direcao,(x-(velocidade*t),y))
                     else if validaPosicaoTerra (x,y+(velocidade*t)) mapa
                     then (Norte,(x,y))
                     else (Sul,(x,y))

-- | Função que atualiza a duração de um projétil com o passar do tempo.
atualizaProjetil :: Tempo -> Projetil -> Projetil
atualizaProjetil t Projetil {duracaoProjetil = Infinita} = Projetil {duracaoProjetil = Infinita}
atualizaProjetil t Projetil {duracaoProjetil = Finita d} = Projetil {duracaoProjetil = Finita (d-t)}

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
               projeteisInimigo = lprojeteisnova}
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
    = (filter (\i -> vidaInimigo i > 0 && posicaoInimigo i /= posicaoBase base) inimigosatualizados, danobase, butim)
    where inimigosaposataque = concatMap (\torre -> if tempoTorre torre == 0 then atacaInimigos torre inimigos else inimigos) torres
          inimigosatualizados = map (atualizaInimigo t mapa) inimigosaposataque -- ainda faltam retirar os mortos e os que chegaram a base
          danobase = getDanoNaBase inimigosatualizados base
          butim = getButim inimigosatualizados

-- | Função que atualiza a base, i.e, a vida da base e os créditos da base.
atualizaBase :: Tempo -> Base -> Float -> Creditos -> Base
atualizaBase t Base {vidaBase = vida, posicaoBase = posicao, creditosBase = creditos} danobase butim
    = Base {vidaBase = vidanova, creditosBase = creditosnovos}
    where vidanova = vida - danobase
          creditosnovos = creditos + butim

-- | Função principal que atualiza o jogo com o passar do tempo.
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t Jogo {baseJogo = base, portaisJogo = lportais, torresJogo = ltorres, inimigosJogo = linimigos, mapaJogo = mapa}
    = Jogo {baseJogo = baseatt, portaisJogo = lportaisatt, torresJogo = ltorresatt, inimigosJogo = linimigosatt}
    where
        lportaisatt = atualizaPortais t lportais
        ltorresatt = atualizaTorres t linimigos ltorres
        (linimigosatt,danonabase,butim) = atualizaInimigos t mapa base ltorresatt linimigos
        baseatt = atualizaBase t base danonabase butim
