module Strategies
( getStrategy1
) where

import System.Random
import Configs

-- Estrategia 01 - onde o numero de tropas eh dividido de forma igualitaria entre os campos
-- O ultimo campo fica com o restante, caso nao seja uma divisao fechada
-- [cb1, cb2, cb3]
-- estrategia1 = [50,50,50]
getStrategy1 :: Int -> Int -> [Int]
getStrategy1 nTropas nCampos =
  [cb1, cb2, cb3]
  where cb1 = quot nTropas nCampos
        cb2 = quot nTropas nCampos
        cb3 = nTropas - (cb1 + cb2)

-- | Apenas para Teste
getStrategy2 :: Int -> Int -> [Int]
getStrategy2 nTropas nCampos = [100,100,100]

-- | Sorteia um numero aleatório entre 1 e o n informado
getNumberRandom n = randomRIO(1, n) :: IO Int

-- | Invoca determinada estrategia, dependendo do numero sorteado
getStrategyForNumber n
 | n == 1    = getStrategy1 numeroDeTropas numeroDeCampos
 -- | n == 2    = getStrategy2 numeroDeTropas numeroDeCampos
 | otherwise = getStrategyForNumber 1

-- | Retorna uma estratégia sorteada do conjunto de estratégias disponíveis
getStrategy = do
 x <- getNumberRandom numeroDeEstrategias
 return $ getStrategyForNumber x
