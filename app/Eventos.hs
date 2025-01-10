module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Tarefa1 (validaPosicaoRelva)
import Data.Maybe (fromJust)
import Dados

{-| Função auxiliar que reage aos eventos do teclado quando o jogo está no menu inicial

Decidimos adicionar este extra com o menu inicial para tornar o jogo mais intuitivo e não aparecer logo o jogo -}

reageEventosMenu :: Event -> ImmutableTowers -> ImmutableTowers
reageEventosMenu (EventKey (SpecialKey KeyRight) Down _ _) it@(ImmutableTowers {menu = ModoJogo EscolherNivel}) = it {nivelAtual = 3} -- ^ cheat code para desbloquear niveis
reageEventosMenu (EventKey (SpecialKey KeyRight) Down _ _) it@(ImmutableTowers {jogoAtual = jogo, menu = ModoJogo EmAndamento}) = it {jogoAtual = jogo {baseJogo = (baseJogo jogo) {creditosBase = 9999}}}
reageEventosMenu (EventKey (MouseButton LeftButton) Down _ posMouse) it@(ImmutableTowers {menu = MenuInicial}) 
    = if clicouDentro botaoJogarMenuInicial posMouse then it {menu = ModoJogo EscolherNivel} else if clicouDentro botaoSairMenuInicial posMouse then error "Jogo fechado." else it
reageEventosMenu (EventKey (MouseButton LeftButton) Down _ posMouse) it@(ImmutableTowers {menu = ModoJogo EscolherNivel})
    = if clicouDentro botaoNivel1 posMouse then it {jogoAtual = jogoInicio01, menu = ModoJogo EmAndamento, torreSelecionadaLoja = Nothing, infoTorre = Nothing}
      else if clicouDentro botaoNivel2 posMouse && (nivelAtual it) >=2 then it {jogoAtual = jogoInicio02, menu = ModoJogo EmAndamento, torreSelecionadaLoja = Nothing, infoTorre = Nothing}
      else if clicouDentro botaoNivel3 posMouse && (nivelAtual it == 3) then it {jogoAtual = jogoInicio03, menu = ModoJogo EmAndamento, torreSelecionadaLoja = Nothing, infoTorre = Nothing}
      else if clicouDentro botaoSairNiveis posMouse then it {menu = MenuInicial}
      else it
reageEventosMenu (EventKey (MouseButton LeftButton) Down _ posMouse) it@(ImmutableTowers {menu = ModoJogo PerdeuJogo}) = 
    if clicouDentro botaoSairMenuPerdeuGanhou posMouse then it {menu = MenuInicial, torreSelecionadaLoja = Nothing, infoTorre = Nothing} else it
reageEventosMenu (EventKey (MouseButton LeftButton) Down _ posMouse) it@(ImmutableTowers {menu = ModoJogo GanhouJogo}) = 
   if clicouDentro botaoSairMenuPerdeuGanhou posMouse 
   then (case nivelAtual it of 
            1 -> it {menu = MenuInicial, torreSelecionadaLoja = Nothing, infoTorre = Nothing, nivelAtual = 2}
            2 -> it {menu = MenuInicial, torreSelecionadaLoja = Nothing, infoTorre = Nothing, nivelAtual = 3}
            _ -> it {menu = MenuInicial, torreSelecionadaLoja = Nothing, infoTorre = Nothing, nivelAtual = 3})
   else it
reageEventosMenu _ it = it

-- | Função auxiliar que reage ao evento do rato quando o jogador clicou dentro da área de melhoria (para simplificar a reageEventos)
reageMelhoriaTorre :: ImmutableTowers -> ImmutableTowers
reageMelhoriaTorre it@(ImmutableTowers {jogoAtual = jogo, menu = ModoJogo EmAndamento}) =
    case infoTorre it of
            Nothing -> it
            Just torre -> if nivelTorre torre <= 3 && podeComprarMelhoriaTorre it
                          then it {jogoAtual = jogo {torresJogo = map (\t -> if (posicaoTorre t) == (posicaoTorre torre) then darUpgradeTorre torre else t) (torresJogo jogo), 
                                                     baseJogo = base {creditosBase = creditosBase base - customelhoriatorre}}, 
                                                     infoTorre = Just (darUpgradeTorre torre)}
                          else it
                           where customelhoriatorre = getCustoMelhoriaTorre it torre
                                 base = baseJogo jogo
reageMelhoriaTorre it = it

{-| Função auxiliar que reage a todos os eventos do rato relacionados a loja:

Isto é, reage quando o jogador clica dentro de uma áreas das torres da loja ou então se clica dentro do mapa (para posicionar a torreSelecionada)

-}

reageCompraTorre :: (Float,Float) -> ImmutableTowers -> ImmutableTowers
reageCompraTorre posMouse it@(ImmutableTowers {jogoAtual = jogo, menu = ModoJogo EmAndamento}) =
    case torreSelecionadaLoja it of
            Nothing -> selecionarTorreLoja posMouse it
            Just torre -> if clicouDentro torreGeloArea posMouse || clicouDentro torreFogoArea posMouse || clicouDentro torreResinaArea posMouse
                          then selecionarTorreLoja posMouse it -- ^ deseleciona a torre selecionada
                          else if podeAdicionarTorre (xc,yc) it && podeComprarTorre it
                          then it {torreSelecionadaLoja = Nothing,jogoAtual = jogo {torresJogo = torresJogo jogo ++ [torrenova], baseJogo = base {creditosBase = creditosBase base - custotorre}}}
                          else it
                          where (xc,yc) = posEcraParaJogo posMouse
                                torrenova = torre {posicaoTorre = (xc,yc)}
                                custotorre = getCustoTorre torre (lojaJogo jogo)
                                base = baseJogo jogo
reageCompraTorre _ it = it

-- | Função principal que reage aos eventos do jogador (ações do teclado e do rato)
reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (MouseButton LeftButton) Down _ posMouse) it@(ImmutableTowers {jogoAtual = jogo, menu = ModoJogo EmAndamento}) =
    if clicouDentro botaoMelhoriaArea posMouse
    then reageMelhoriaTorre it
    else if clicouDentro torreGeloArea posMouse || clicouDentro torreFogoArea posMouse || clicouDentro torreResinaArea posMouse || clicouDentro mapaArea posMouse
    then reageCompraTorre posMouse it
    else it
reageEventos (EventKey (MouseButton RightButton) Down _ posMouse) it@(ImmutableTowers {jogoAtual = jogo, infoTorre = info, menu = ModoJogo EmAndamento}) =
    case clicouEmTorre posMouse (torresJogo jogo) of
        Just t -> case info of
                    Nothing -> it {infoTorre = Just t}
                    Just t' -> if (posicaoTorre t') == (posicaoTorre t) then it {infoTorre = Nothing} else it {infoTorre = Just t}
        Nothing -> it {infoTorre = Nothing}
reageEventos k it@(ImmutableTowers {menu = MenuInicial}) = reageEventosMenu k it
reageEventos k it@(ImmutableTowers {menu = ModoJogo _}) = reageEventosMenu k it
reageEventos _ it = it


{-| Funções auxiliares:

As funções que tem "Area" são funções que retornam o (xmin,xmax,ymin,ymax) das áreas destas imagens

Obtivemos as coordenadas destas áreas fazendo o trace na função reageEventos para obter a posição do clique do rato -}

torreGeloArea :: (Float, Float, Float, Float)
torreGeloArea = (-911, -728, 111.5, 294.5)

torreFogoArea :: (Float, Float, Float, Float)
torreFogoArea = (-911,-728,-118.5,65.5)

torreResinaArea :: (Float, Float, Float, Float)
torreResinaArea = (-911,-728,-346.5,-163.5)

mapaArea :: (Float, Float, Float, Float)
mapaArea = (-440, 440, -495, 385)

botaoJogarMenuInicial :: (Float, Float, Float, Float)
botaoJogarMenuInicial = (-215,215,-24.5,68.5)

botaoSairMenuInicial :: (Float, Float, Float, Float)
botaoSairMenuInicial = (-215,215,-191.5,-98.5)

botaoSairMenuPerdeuGanhou :: (Float, Float, Float, Float)
botaoSairMenuPerdeuGanhou = (-215,215,-171.5,-78.5)

botaoNivel1 :: (Float, Float, Float, Float)
botaoNivel1 = (-215,215,157.5,250.5)

botaoNivel2 :: (Float, Float, Float, Float)
botaoNivel2 = (-215,215,22.5,115.5)

botaoNivel3 :: (Float, Float, Float, Float)
botaoNivel3 = (-215,215,-112.5,-19.5)

botaoSairNiveis :: (Float, Float, Float, Float)
botaoSairNiveis = (-215,215,-247.5,-155.5)

botaoMelhoriaArea :: (Float, Float, Float, Float)
botaoMelhoriaArea = (608, 909, 124.5, 264.5)


{-| Função auxiliar que recebe o (xmin,xmax,ymin,ymax) de uma área e recebe (x,y) coordenadas do clique do rato e 
verifica se o clique do rato está dentro desta área -}

clicouDentro :: (Float, Float, Float, Float) -> (Float, Float) -> Bool
clicouDentro (xmin, xmax, ymin, ymax) (x, y) = x >= xmin && x <= xmax && y >= ymin && y <= ymax

-- | Função que retorna o custo de uma determinada torre na loja
getCustoTorre :: Torre -> Loja -> Creditos
getCustoTorre torre loja = let inverteTupla (c,t) = (t,c)
                               lojainvertida = map inverteTupla loja
                           in fromJust (lookup torre lojainvertida)

-- | Função que verifica se o jogador pode comprar uma torre (se tem créditos suficientes)
podeComprarTorre :: ImmutableTowers -> Bool
podeComprarTorre it = case torreSelecionadaLoja it of
                         Nothing -> False
                         Just torre -> creditosBase (baseJogo (jogoAtual it)) >= getCustoTorre torre (lojaJogo (jogoAtual it))

-- | Função que retorna o custo de uma determinada melhoria de torre
getCustoMelhoriaTorre :: ImmutableTowers -> Torre -> Creditos
getCustoMelhoriaTorre it torre = let nivelatual = nivelTorre torre
                                     tipoTorre = tipoProjetil (projetilTorre torre)
                                     listaprecos = (precoUpgrades (jogoAtual it))
                                     fstTripla (c,_,_) = c
                                 in fstTripla $ head (filter (\(c,n,t) -> n == nivelatual && t == tipoTorre) listaprecos)


-- | Função que verifica se o jogador pode comprar uma torre (se tem créditos suficientes)
podeComprarMelhoriaTorre :: ImmutableTowers -> Bool
podeComprarMelhoriaTorre it = case infoTorre it of
                         Nothing -> False
                         Just torre -> if nivelTorre torre == 4 
                                       then False
                                       else creditosBase (baseJogo (jogoAtual it)) >= getCustoMelhoriaTorre it torre

-- | Função que seleciona a torre que o jogador clicou na loja (ou deseleciona se já estava selecionada)
selecionarTorreLoja :: (Float, Float) -> ImmutableTowers -> ImmutableTowers
selecionarTorreLoja (x, y) it
    | clicouDentro torreGeloArea (x, y) = if torreSelecionadaLoja it == Just torregelo 
                                          then it {torreSelecionadaLoja = Nothing} 
                                          else it {torreSelecionadaLoja = Just torregelo}
    | clicouDentro torreFogoArea (x,y) = if torreSelecionadaLoja it == Just torrefogo
                                         then it {torreSelecionadaLoja = Nothing}
                                         else it {torreSelecionadaLoja = Just torrefogo}
    | clicouDentro torreResinaArea (x,y) = if torreSelecionadaLoja it == Just torreresina
                                           then it {torreSelecionadaLoja = Nothing}
                                           else it {torreSelecionadaLoja = Just torreresina}
    | otherwise = it { torreSelecionadaLoja = Nothing }
    where loja = lojaJogo (jogoAtual it)
          torregelo = head (filter (\t -> tipoProjetil (projetilTorre t) == Gelo) (map (\(_,t) -> t) loja))
          torrefogo = head (filter (\t -> tipoProjetil (projetilTorre t) == Fogo) (map (\(_,t) -> t) loja))
          torreresina = head (filter (\t -> tipoProjetil (projetilTorre t) == Resina) (map (\(_,t) -> t) loja))

-- | Função auxiliar que converte as coordenadas de um clique do rato (que está situado dentro do mapa) para as coordenadas do jogo
posEcraParaJogo :: (Float, Float) -> (Float, Float)
posEcraParaJogo (x, y) = let tamanhoQuadrado = 80 -- (visto que o w e o h são ambos 80)
                             indiceX = floor ((x + 440) / tamanhoQuadrado) :: Int
                             indiceY = floor ((385 - y) / tamanhoQuadrado) :: Int
                             posJogo = (fromIntegral indiceX + 0.5, fromIntegral indiceY + 0.5)
                         in posJogo

-- | Função que verifica se é possível adicionar uma torre numa determinada posição (deve ser uma posição de relva e não deve haver outra torre nessa posição)
podeAdicionarTorre :: (Float, Float) -> ImmutableTowers -> Bool
podeAdicionarTorre pos it =
    let torres = torresJogo (jogoAtual it)
        posicoesExistentesTorres = map posicaoTorre torres
    in not (elem pos posicoesExistentesTorres) && validaPosicaoRelva pos (mapaJogo (jogoAtual it))

-- | Função que verifica se o jogador clicou em alguma torre do mapa
clicouEmTorre :: Posicao -> [Torre] -> Maybe Torre
clicouEmTorre _ [] = Nothing
clicouEmTorre posMouse (torre:resto) =
    if clicouDentro mapaArea posMouse
    then (if (posEcraParaJogo posMouse) == posicaoTorre torre then Just torre else clicouEmTorre posMouse resto)
    else Nothing

{-| Função que da upgrade a uma torre

Neste caso, nós decidimos que as melhorias para cada nível seriam estas mudanças, mas poderiam ser outras -}

darUpgradeTorre :: Torre -> Torre
darUpgradeTorre torre = case nivelTorre torre of
                            1 -> torre {danoTorre = danoTorre torre + 10, nivelTorre = nivelTorre torre + 1}
                            2 -> torre {danoTorre = danoTorre torre + 10, nivelTorre = nivelTorre torre + 1}
                            3 -> torre {danoTorre = danoTorre torre + 10, alcanceTorre = alcanceTorre torre + 1, nivelTorre = nivelTorre torre + 1}
                            _ -> torre