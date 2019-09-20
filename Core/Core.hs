module Core.Core
( play
) where

import Lens.Micro((^.))

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
getWin :: [Int] -> [Int] -> Vencedor
getWin jogador coronel
  | vJogador == vCoronel = EMPATE
  | vJogador > vCoronel  = JOGADOR
  | otherwise            = CORONEL
  where vJogador = countWins lista JOGADOR
        vCoronel = countWins lista CORONEL
        lista    = getListWinners jogador coronel

play :: AppState -> Vencedor
play st = getWin (st^.fields) (getStrategy (st^.quantitySoldiers) (getQtdFields $ st^.qtdFields))
