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

dicas =
  [ "O coronel Blotto é um jogo de soma zero, o seu ganho é diretamente proporcional a perda do seu adversário"
  , "O coronel Blotto é um jogo não-cooperativo, você não colabora para que o seu adversário ganhe, mas sim busca uma estratégia para ganhar dele"
  ]
