module Core.Core
( play
) where

import Lens.Micro((^.), (&), (.~))

import Core.Strategies
import Types

-- |Dado o numero de tropas do jogador e do coronel, retorna o vencedor desse campo ou um empate
getWinField :: Int -> Int -> Vencedor
getWinField a b
 | a == b = EMPATE
 | a > b = JOGADOR
 | otherwise = CORONEL

-- |Recebe as duas jogadas e decide quem foi o vitorioso em cada campo de batalha
getListWinners :: [Int] -> [Int] -> [Vencedor]
getListWinners [] [] = [EMPATE]
getListWinners [a] [b] = [getWinField a b]
getListWinners (x:xs) (y:ys) = getListWinners [x] [y] ++ getListWinners xs ys

countWins :: [Vencedor] -> Vencedor -> Int
countWins lista tipo
 | tipo == JOGADOR = length $ filter (==JOGADOR) lista
 | tipo == CORONEL = length $ filter (==CORONEL) lista
 | otherwise       = 0

-- |Recebe as duas jogadas e decide quem eh o vencedor
getWinner :: [Int] -> [Int] -> Vencedor
getWinner jogador coronel
  | vJogador < vCoronel = CORONEL
  | vJogador > vCoronel = JOGADOR
  | otherwise           = EMPATE
  where vJogador = countWins lista JOGADOR
        vCoronel = countWins lista CORONEL
        lista    = getListWinners jogador coronel

play :: AppState -> AppState
play st = st'
  where strategy = getStrategy (st^.quantitySoldiers) (getQtdFields $ st^.qtdFields)
        w = getWinner (st^.fields) strategy
        st' = st & fieldsBlotto .~ strategy
                 & winner .~ w
