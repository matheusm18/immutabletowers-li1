# Extras implementados

## Tipos de inimigos diferentes (Extra 3)

No momento, existem dois tipos de inimigo: o inimigo normal (soldado) e um inimigo mais forte (Blindado), sendo este último imune aos efeitos dos projéteis que as torres atiram, ou seja, sofrem apenas o dano relativo ao impacto do projétil da torre.

Para efeito, adicionamos um campo ao data type dos Inimigos chamado tipoInimigo que guarda o tipo de cada inimigo do jogo.

## Sistema de melhorias a torres (Extra 5)

Também adicionamos o extra de tornar possivel melhorar as torres para um estado mais poderoso (mantendo o mesmo tipo de projétil) por uma certa quantidade de créditos de modo a tornar o jogo mais dinâmico e possibilitar novas estratégias.

Para efeito, basta clicar com o botao direito do rato em cima de uma torre já posicionada e irá aparecer as informações no canto superior direito sobre o nível atual da torre, os benefícios de melhorar a torre para o próximo nível e também o custo para realizar a melhoria. 

Para guardar o nível atual da torre foi adicionado no data type das torres o campo nivelTorre que guarda o nivel atual da torre naquele momento, possibilitando assim depois verificações nas funções relacionadas a este sistema.

## Sistema de progressão de jogo (Níveis - Extra 9)

Adicionamos um sistema de progressão de jogo, onde após vencer o primeiro nível, é desbloqueado o próximo nível. A cada nível que passa a dificuldade do jogo aumenta, de modo a tornar o jogo mais dinâmico.

No momento, existem 3 níveis disponíveis e cada nível conta com mapa e inimigos únicos.

Para possibilitar este sistema, adicionamos no data type do ImmutableTowers um campo chamado nivelAtual que guarda o nível máximo da fase em que o jogador pode jogar no momento. Também optamos por criar um arquivo chamado Dados.hs onde é armazenado os jogos iniciais de cada nível.

## Outras funcionalidades gráficas

Além dos outros extras implementados, também achamos interessante adicionar um menu inicial onde o jogador pode escolher a opção jogar e a opção sair. Se o jogador seleciona jogar irá ser disponibilizado a lista de níveis disponiveis no momento para jogar e se selecionar sair, o jogo irá fechar.

Outra funcionalidade gráfica adicionada foi o efeito de partículas nos inimigos, isto é, quando o inimigo está sobre o efeito de algum projétil, a sua imagem será representada consoante o efeito ativo nele. Se estiver sobre efeito de fogo, haverá chamas em volta do inimigo, se estiver sobre efeito do gelo haverá gelo em volta e se estiver sobre efeito da resina haverá resina em volta.

No caso do inimigo estar com resina e gelo simultaneamente na lista de projéteis, optamos por aplicar a imagem do inimigo com gelo, visto que, se ele estiver congelado, ele nem sequer irá andar, ou seja, não iria fazer sentido aplicar a imagem do inimigo com a resina.