module Core.Strategies
( getStrategy
) where

import Lens.Micro

import Configs(strategies)
import Types

-- | Estrategia 01 - onde o numero de tropas eh dividido de forma igualitaria entre os campos
-- O ultimo campo fica com o restante, caso nao seja uma divisao fechada
-- 100 = [33,33,34]
getStrategy1 :: Int -> Int -> [Int]
getStrategy1 nTropas 3 = [cb1, cb2, cb3]
  where cb1 = quot nTropas 3
        cb2 = quot nTropas 3
        cb3 = nTropas - (cb1 + cb2)
-- 100 = [25, 25, 25, 25]
getStrategy1 nTropas 4 = [cb1, cb2, cb3, cb4]
  where cb1 = quot nTropas 4
        cb2 = quot nTropas 4
        cb3 = quot nTropas 4
        cb4 = nTropas - (cb1 + cb2 + cb3)
-- 100 = [20, 20, 20, 20, 20]
getStrategy1 nTropas 5 = [cb1, cb2, cb3, cb4, cb5]
  where cb1 = quot nTropas 5
        cb2 = quot nTropas 5
        cb3 = quot nTropas 5
        cb4 = quot nTropas 5
        cb5 = nTropas - (cb1 + cb2 + cb3 + cb4)

-- | Estrategia 02 - onde o numero de tropas eh dividido de forma igualitaria entre a metade
-- dos campos de batalha, o restante fica zerado
-- 100 = [50, 50, 0]
getStrategy2 :: Int -> Int -> [Int]
getStrategy2 nTropas 3 = [cb1, cb2, 0]
  where cb1 = quot nTropas 2
        cb2 = nTropas - cb1
-- 100 = [50, 50, 0, 0]
getStrategy2 nTropas 4 = [cb1, cb2, 0, 0]
  where cb1 = quot nTropas 2
        cb2 = nTropas - cb1
-- 100 = [33, 33, 34, 0, 0]
getStrategy2 nTropas 5 = (getStrategy1 nTropas 3) ++ [0, 0]

-- | Estrategia 03 - onde o numero de tropas eh dividido 50% para o ultimo campo, o restante
-- dividido de forma igualitaria entre o restante dos campos
-- 100 = [25, 25, 50]
getStrategy3 :: Int -> Int -> [Int]
getStrategy3 nTropas 3 = [cb1, cb2, cb3]
  where cb3 = quot nTropas 2
        cb1 = quot (nTropas - cb3) 2
        cb2 = nTropas - (cb1 + cb3)
-- 100 = [16, 16, 18, 50]
getStrategy3 nTropas 4 = [cb1, cb2, cb3, cb4]
  where cb4 = quot nTropas 2
        cb1 = quot (nTropas - cb4) 3
        cb2 = cb1
        cb3 = nTropas - (cb1 + cb2 + cb4)
-- 100 = [12, 12, 12, 14, 50]
getStrategy3 nTropas 5 = [cb1, cb2, cb3, cb4, cb5]
  where cb5 = quot nTropas 2
        cb1 = quot (nTropas - cb5) 4
        cb2 = cb1
        cb3 = cb2
        cb4 = nTropas - (cb1 + cb2 + cb3 + cb5)

-- | Estrategia 04 - onde o numero de tropas eh dividido totalmente aleatorio,
-- sem nenhum calculo
getStrategy4 :: Int -> Int -> [Int]
getStrategy4 100 3 = [5, 40, 55]
getStrategy4 150 3 = [15, 35, 100]
getStrategy4 200 3 = [100, 100, 0]
getStrategy4 100 4 = [20, 20, 40, 20]
getStrategy4 150 4 = [90, 20, 20, 20]
getStrategy4 200 4 = [80, 20, 80, 20]
getStrategy4 100 5 = [45, 35, 10, 0, 10]
getStrategy4 150 5 = [40, 40, 50, 0, 20]
getStrategy4 200 5 = [50, 50, 70, 30, 0]

-- | Estrategia 05 - onde o numero de tropas eh dividido buscando a maioria dos campos
-- 100 = [50, 50, 0]
getStrategy5 :: Int -> Int -> [Int]
getStrategy5 nTropas 3 = getStrategy2 nTropas 3
-- 100 = [33, 33, 34, 0]
getStrategy5 nTropas 4 = getStrategy1 nTropas 3 ++ [0]
-- 100 = [33, 33, 34, 0, 0]
getStrategy5 nTropas 5 = getStrategy2 nTropas 5

-- | Invoca determinada estrategia, dependendo do numero sorteado
getStrategyForNumber :: Int -> Int -> Int -> [Int]
getStrategyForNumber n nTropas nCampos =
  case n of
    1 -> getStrategy1 nTropas nCampos
    2 -> getStrategy2 nTropas nCampos
    3 -> getStrategy3 nTropas nCampos
    4 -> getStrategy4 nTropas nCampos
    _ -> getStrategy5 nTropas nCampos

-- | Retorna uma estratégia sorteada do conjunto de estratégias disponíveis
getStrategy :: AppState -> [Int]
getStrategy st = getStrategyForNumber (strategies !! (st^.currentConcept)) (st^.quantitySoldiers) (getQtdFields (st^.qtdFields))
