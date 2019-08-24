module Strategies
( getStrategy1
) where

-- Estrategia 01 - onde o numero de tropas eh dividido de forma igualitaria entre os campos
-- O ultimo campo fica com o restante, caso nao seja uma divisao fechada
-- [cb1, cb2, cb3]
getStrategy1 numeroDeTropas numeroDeCampos =
  [cb1, cb2, cb3]
  where cb1 = quot numeroDeTropas numeroDeCampos
        cb2 = quot numeroDeTropas numeroDeCampos
        cb3 = numeroDeTropas - (cb1 + cb2)
-- estrategia1 = [50,50,50]
