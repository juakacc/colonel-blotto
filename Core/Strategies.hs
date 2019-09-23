module Core.Strategies
( getStrategy
) where

import System.Random
import Configs(numberOfStrategies)

-- Estrategia 01 - onde o numero de tropas eh dividido de forma igualitaria entre os campos
-- O ultimo campo fica com o restante, caso nao seja uma divisao fechada
getStrategy1 :: Int -> Int -> [Int]
getStrategy1 nTropas 3 = [cb1, cb2, cb3]
  where cb1 = quot nTropas 3
        cb2 = quot nTropas 3
        cb3 = nTropas - (cb1 + cb2)

getStrategy1 nTropas 4 = [cb1, cb2, cb3, cb4]
  where cb1 = quot nTropas 4
        cb2 = quot nTropas 4
        cb3 = quot nTropas 4
        cb4 = nTropas - (cb1 + cb2 + cb3)

getStrategy1 nTropas 5 = [cb1, cb2, cb3, cb4, cb5]
  where cb1 = quot nTropas 5
        cb2 = quot nTropas 5
        cb3 = quot nTropas 5
        cb4 = quot nTropas 5
        cb5 = nTropas - (cb1 + cb2 + cb3 + cb4)

-- | Estrategia 02 - onde o numero de tropas eh dividido de forma igualitaria entre a metade
-- dos campos de batalha, o restante fica zerado
-- [cb1, cb2, cb3]
-- estrategia2 = [75, 75, 0]
getStrategy2 :: Int -> Int -> [Int]
getStrategy2 nTropas 3 = [cb1, cb2, 0]
  where cb1 = quot nTropas 2
        cb2 = nTropas - cb1

getStrategy2 nTropas 4 = [cb1, cb2, 0, 0]
  where cb1 = quot nTropas 2
        cb2 = nTropas - cb1

getStrategy2 nTropas 5 = (getStrategy1 nTropas 3) ++ [0, 0]

-- getStrategy3 :: Int -> Int -> [Int]
-- getStrategy3 nTropas 3 = [cb1, cb2, cb3]
--   where cb1 = 

-- | Sorteia um numero aleatório entre 1 e o n informado
getNumberRandom :: Int -> IO Int
getNumberRandom n = randomRIO(1, n) :: IO Int

-- | Invoca determinada estrategia, dependendo do numero sorteado
getStrategyForNumber :: Int -> Int -> Int -> [Int]
getStrategyForNumber n nTropas nCampos
 | n == 1    = getStrategy1 nTropas nCampos
 | n == 2    = getStrategy2 nTropas nCampos
 | otherwise = getStrategyForNumber 1 nTropas nCampos

--
-- getStra :: Int -> Int -> [Int]
-- getStra nTropas nCampos = do
--   x <- getNumberRandom numberOfStrategies
--   case x of
--     1 -> getStrategy1 nTropas nCampos
--     _ -> getStrategy2 nTropas nCampos

-- | Retorna uma estratégia sorteada do conjunto de estratégias disponíveis
getStrategy :: Int -> Int -> [Int]
getStrategy nTropas nCampos = getStrategyForNumber 1 nTropas nCampos
 --  getStrategyForNumber x nTropas nCampos
 --  where x = getNumberRandom numberOfStrategies
 -- --  do
 -- let x
 -- getStrategyForNumber x nTropas nCampos
