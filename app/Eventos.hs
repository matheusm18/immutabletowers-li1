module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Tarefa1 (validaPosicaoRelva)
import Data.Maybe (fromJust)
import Debug.Trace

{-| Função auxiliar que reage aos eventos do teclado quando o jogo está no menu inicial

Decidimos adicionar este extra com o menu inicial para tornar o jogo mais intuitivo e não aparecer logo o jogo -}
reageEventosMenu :: Event -> ImmutableTowers -> ImmutableTowers
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {menu = MenuInicial Jogar}) = it {menu = ModoJogo EmAndamento}
reageEventosMenu (EventKey (MouseButton LeftButton) Down _ posMouse) it@(ImmutableTowers {menu = MenuInicial Jogar}) = if clicouDentro botaoJogarMenuInicial posMouse then it {menu = ModoJogo EmAndamento} else if clicouDentro botaoSairMenuInicial posMouse then error "Jogo fechado." else it
reageEventosMenu (EventKey (SpecialKey KeyDown) Down _ _) it@(ImmutableTowers {menu = MenuInicial Jogar}) = it {menu = MenuInicial Sair}
reageEventosMenu (EventKey (SpecialKey KeyUp) Down _ _) it@(ImmutableTowers {menu = MenuInicial Sair}) = it {menu = MenuInicial Jogar}
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {jogoInicial = jogoInicio, menu = ModoJogo PerdeuJogo}) = it {jogoAtual = jogoInicio, menu = MenuInicial Jogar, torreSelecionada = Nothing, infoTorre = Nothing}
reageEventosMenu (EventKey (MouseButton LeftButton) Down _ posMouse) it@(ImmutableTowers {jogoInicial = jogoInicio, menu = ModoJogo PerdeuJogo}) = if clicouDentro botaoSairMenuPerdeu posMouse then it {jogoAtual = jogoInicio, menu = MenuInicial Jogar, torreSelecionada = Nothing, infoTorre = Nothing} else it
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {jogoInicial = jogoInicio, menu = ModoJogo GanhouJogo}) = it {jogoAtual = jogoInicio, menu = MenuInicial Jogar, torreSelecionada = Nothing, infoTorre = Nothing}
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) (ImmutableTowers {menu = MenuInicial Sair})  = error "Jogo fechado."
reageEventosMenu _ it = it

-- | Função principal que reage aos eventos do jogador (ações do teclado e do rato)
reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (MouseButton LeftButton) Down _ posMouse) it@(ImmutableTowers {jogoAtual = jogo, menu = ModoJogo EmAndamento}) =
    trace (show posMouse) $
    if clicouDentro botaoMelhoriaArea posMouse
    then case infoTorre it of
            Nothing -> it
            Just torre -> if nivelTorre torre <= 3 && podeComprarMelhoriaTorre it
                          then it {jogoAtual = jogo {torresJogo = map (\t -> if t == torre then darUpgradeTorre torre else t) (torresJogo jogo), 
                                                     baseJogo = base {creditosBase = creditosBase base - customelhoriatorre}}, 
                                                     infoTorre = Just (darUpgradeTorre torre)}
                          else it
                           where customelhoriatorre = getCustoMelhoriaTorre it torre
                                 base = baseJogo jogo
    else
    case torreSelecionada it of 
            Nothing -> selecionarTorre posMouse it
            Just torre -> case posEcraParaJogo posMouse of
                            Nothing -> selecionarTorre posMouse it
                            Just (xc,yc) -> if podeAdicionarTorre (xc,yc) it && podeComprarTorre it
                                            then it {torreSelecionada = Nothing,jogoAtual = jogo {torresJogo = torresJogo jogo ++ [torrenova], baseJogo = base {creditosBase = creditosBase base - custotorre}}}
                                            else it
                                             where torrenova = torre {posicaoTorre = (xc,yc)}
                                                   custotorre = getCustoTorre torre (lojaJogo jogo)
                                                   base = baseJogo jogo
reageEventos (EventKey (MouseButton RightButton) Down _ posMouse) it@(ImmutableTowers {jogoAtual = jogo, infoTorre = info, menu = ModoJogo EmAndamento}) =
    case clicouEmTorre posMouse (torresJogo jogo) of
        Just t -> case info of
                    Nothing -> it {infoTorre = Just t}
                    Just t' -> if t' == t then it {infoTorre = Nothing} else it {infoTorre = Just t}
        Nothing -> it {infoTorre = Nothing}
reageEventos k it@(ImmutableTowers {menu = MenuInicial m}) = reageEventosMenu k (it {menu = MenuInicial m})
reageEventos k it@(ImmutableTowers {menu = ModoJogo m}) = reageEventosMenu k (it {menu = ModoJogo m})
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

botaoSairMenuPerdeu :: (Float, Float, Float, Float)
botaoSairMenuPerdeu = (-215,215,-171.5,-78.5)

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
podeComprarTorre it = case torreSelecionada it of
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

-- | Função que seleciona a torre que o jogador clicou (ou deseleciona se já estava selecionada)
selecionarTorre :: (Float, Float) -> ImmutableTowers -> ImmutableTowers
selecionarTorre (x, y) it
    | clicouDentro torreGeloArea (x, y) = if torreSelecionada it == Just torregelo 
                                          then it {torreSelecionada = Nothing} 
                                          else it {torreSelecionada = Just torregelo}
    | clicouDentro torreFogoArea (x,y) = if torreSelecionada it == Just torrefogo
                                         then it {torreSelecionada = Nothing}
                                         else it {torreSelecionada = Just torrefogo}
    | clicouDentro torreResinaArea (x,y) = if torreSelecionada it == Just torreresina
                                           then it {torreSelecionada = Nothing}
                                           else it {torreSelecionada = Just torreresina}
    | otherwise = it { torreSelecionada = Nothing }
    where loja = lojaJogo (jogoAtual it)
          torregelo = head (filter (\t -> tipoProjetil (projetilTorre t) == Gelo) (map (\(_,t) -> t) loja))
          torrefogo = head (filter (\t -> tipoProjetil (projetilTorre t) == Fogo) (map (\(_,t) -> t) loja))
          torreresina = head (filter (\t -> tipoProjetil (projetilTorre t) == Resina) (map (\(_,t) -> t) loja))

-- | Função auxiliar que converte as coordenadas do clique do rato no ecrã para as coordenadas do jogo
posEcraParaJogo :: (Float, Float) -> Maybe (Float, Float)
posEcraParaJogo (x, y)
    | clicouDentro mapaArea (x, y) = let tamanhoQuadrado = 80 -- (visto que o w e o h são ambos 80)
                                         indiceX = floor ((x + 440) / tamanhoQuadrado) :: Int
                                         indiceY = floor ((385 - y) / tamanhoQuadrado) :: Int
                                         posicaoJogoX = fromIntegral indiceX + 0.5 -- para retornar a posição do jogo do centro do quadrado
                                         posicaoJogoY = fromIntegral indiceY + 0.5
                                     in Just (posicaoJogoX, posicaoJogoY)
    | otherwise = Nothing

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
    case posEcraParaJogo posMouse of
        Just (x,y) -> if (x,y) == (posicaoTorre torre) then Just torre else clicouEmTorre posMouse resto
        Nothing -> Nothing

{-| Função que da upgrade a uma torre

Neste caso, nós decidimos que as melhorias para cada nível seriam estas mudanças, mas poderiam ser outras -}

darUpgradeTorre :: Torre -> Torre
darUpgradeTorre torre = case nivelTorre torre of
                            1 -> torre {danoTorre = danoTorre torre + 10, nivelTorre = nivelTorre torre + 1}
                            2 -> torre {danoTorre = danoTorre torre + 10, nivelTorre = nivelTorre torre + 1}
                            3 -> torre {danoTorre = danoTorre torre + 10, alcanceTorre = alcanceTorre torre + 1, nivelTorre = nivelTorre torre + 1}
                            _ -> torre