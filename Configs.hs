module Configs
( numeroDeCampos
, numeroDeTropas
, numeroDeEstrategias
, dicas
) where

-- Configurações gerais do jogo

numeroDeCampos :: Int
numeroDeCampos = 3

numeroDeTropas :: Int
numeroDeTropas = 150

numeroDeEstrategias :: Int
numeroDeEstrategias = 1


-- 10 de setembro, 15:25

dicas =
  [ "O coronel Blotto é um jogo de soma zero, o seu ganho é diretamente proporcional a perda do seu adversário"
  , "O coronel Blotto é um jogo não-cooperativo, você não colabora para que o seu adversário ganhe, mas sim busca uma estratégia para ganhar dele"
  , "O coronel Blotto é um jogo simultâneo, você distribui as suas tropas ao mesmo tempo que o seu adversário também distribui"
  , "O coronel Blotto é um jogo de informação imperfeita, você não tem informações concretas do seu adversário anteriores ao jogo, trabalha com suposições"
  , "O coronel Blotto é um jogo estático, uma partida é feita com apenas uma jogada, uma disposição de tropas"
  , "O coronel Blotto é um jogo simétrico, se você trocar a sua estratégia pela do seu adversário e ele fizer o mesmo com a sua, os resultados serão equivalentes"
  , "O coronel Blotto é um jogo de informação completa, já que você sabe todas as regras e quais os possíveis resultados dependendo da estratégia utilizada"
  , "O coronel Blotto é um jogo indeterminado, não há uma estratégia para você garantir no mínimo um empate, seu adversário é imprevisível"
  , "---"
  , "A teoria dos jogos estuda as interações dos jogadores a fim de escolher decisões ótimas para eles, de acordo com os seus interesses"
  , "Estratégias são um conjunto de opções de ação que você tem para chegar a um conjunto de resultados"
  , "Estratégia pura é quando você não realiza cálculos para escolher, escolhe de forma natural"
  , "Estratégia dominante é quando você tem uma estratégia que sempre vai ser a melhor, independente das escolhas dos demais jogadores"
  , "Estratégia mista é quando você escolhe se baseando em algum cálculo, com o objetivo de diminuir os riscos"
  , "Payoff é a recompensa que você recebe ao final do jogo, ganhando ou perdendo"
  , "Função de utilidade é a função utilizada para calcular o seu payoff e dos demais jogadores"
  , "Equilíbrio de Nash é quando todos os jogadores escolhem suas melhores estratégias, tendo em vista as escolhas dos demais jogadores"
  ]
