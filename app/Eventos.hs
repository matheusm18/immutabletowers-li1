module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Tarefa1 (validaPosicaoRelva)
import Data.Maybe (fromJust)

-- | Função auxiliar que reage aos eventos do teclado quando o jogo está no menu inicial
reageEventosMenu :: Event -> ImmutableTowers -> ImmutableTowers
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {menu = MenuInicial Jogar}) = it {menu = ModoJogo EmAndamento}
reageEventosMenu (EventKey (SpecialKey KeyDown) Down _ _) it@(ImmutableTowers {menu = MenuInicial Jogar}) = it {menu = MenuInicial Sair}
reageEventosMenu (EventKey (SpecialKey KeyUp) Down _ _) it@(ImmutableTowers {menu = MenuInicial Sair}) = it {menu = MenuInicial Jogar}
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {jogoInicial = jogoInicio, menu = ModoJogo PerdeuJogo}) = it {jogoAtual = jogoInicio, menu = MenuInicial Jogar}
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {jogoInicial = jogoInicio, menu = ModoJogo GanhouJogo}) = it {jogoAtual = jogoInicio, menu = MenuInicial Jogar}
reageEventosMenu (EventKey (SpecialKey KeyEnter) Down _ _) it@(ImmutableTowers {menu = MenuInicial Sair})  = error "Jogo fechado."
reageEventosMenu _ it = it

-- | Função principal que reage aos eventos do jogador (ações do teclado e do rato)
reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (MouseButton LeftButton) Down _ posMouse) it@(ImmutableTowers {jogoAtual = jogo}) =
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
reageEventos k it@(ImmutableTowers {menu = MenuInicial m}) = reageEventosMenu k (it {menu = MenuInicial m})
reageEventos k it@(ImmutableTowers {menu = ModoJogo m}) = reageEventosMenu k (it {menu = ModoJogo m})
reageEventos _ it = it


{- Funções auxiliares:

As funções que tem "Area" são funções que retornam o (xmin,xmax,ymin,ymax) das áreas destas imagens

Obtivemos as coordenadas destas áreas fazendo o trace na função reageEventos para obter a posição do clique do rato -}

torreGeloArea :: (Float, Float, Float, Float)
torreGeloArea = (-911, -665, 46.5, 294.5)

torreFogoArea :: (Float, Float, Float, Float)
torreFogoArea = (-911,-665,-221.5,30.5)

torreResinaArea :: (Float, Float, Float, Float)
torreResinaArea = (-911,-665,-491.5,-241.5)

mapaArea :: (Float, Float, Float, Float)
mapaArea = (-320, 320, -400, 240)

{- Função auxiliar que recebe o (xmin,xmax,ymin,ymax) de uma área e recebe (x,y) coordenadas do clique do rato e 
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
          torregelo = head (filter (\t -> tipoProjetil (projetilTorre t) == Gelo) (map (\(c,t) -> t) loja))
          torrefogo = head (filter (\t -> tipoProjetil (projetilTorre t) == Fogo) (map (\(c,t) -> t) loja))
          torreresina = head (filter (\t -> tipoProjetil (projetilTorre t) == Resina) (map (\(c,t) -> t) loja))

-- | Função auxiliar que converte as coordenadas do clique do rato no ecrã para as coordenadas do jogo
posEcraParaJogo :: (Float, Float) -> Maybe (Float, Float)
posEcraParaJogo (x, y)
    | clicouDentro mapaArea (x, y) = let tamanhoQuadrado = 80 -- (visto que o w e o h são ambos 80)
                                         indiceX = floor ((x + 320) / tamanhoQuadrado)
                                         indiceY = floor ((240 - y) / tamanhoQuadrado)
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