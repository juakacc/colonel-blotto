import Control.Monad

import Core.Strategies
import Testes.Menus
import Configs
import Types (Value(..))

-- |Recupera um numero inteiro do console
getNumero = do
  l <- readLn :: IO Int
  return l

getNumeroDeTropas limite =
  if (limite == 0) then return 0
                   else do
                     n <- getNumero
                     if n <= limite then return n
                                    else
                                      getNumeroDeTropas limite

-- |Dado o numero de tropas do jogador e do coronel, retorna o vencedor desse campo ou um empate
getV a b
 | a == b = EMPATE
 | a > b = JOGADOR
 | otherwise = CORONEL

-- |Recebe as duas jogadas e decide quem foi o vitorioso em cada campo de batalha
getList [] [] = [EMPATE]
getList [a] [b] = [getV a b]
getList (x:xs) (y:ys) = getList [x] [y] ++ getList xs ys

contarVitorias lista tipo
 | tipo == JOGADOR = length $ filter (==JOGADOR) lista
 | tipo == CORONEL = length $ filter (==CORONEL) lista
 | otherwise       = 0

-- |Recebe as duas jogadas e decide quem eh o vencedor
getVencedor jogador coronel
         | vJogador == vCoronel = EMPATE
         | vJogador > vCoronel  = JOGADOR
         | otherwise            = CORONEL
         where vJogador = contarVitorias lista JOGADOR
               vCoronel = contarVitorias lista CORONEL
               lista    = getList jogador coronel

getJogador = do
  putStr "Quantas tropas para o campo de batalha 1? "
  l1 <- getNumero
  putStr "Quantas tropas para o campo de batalha 2? "
  l2 <- getNumero
  putStr "Quantas tropas para o campo de batalha 3? "
  l3 <- getNumero
  return [l1, l2, l3]

pegarDados = do
  jogada <- getJogador -- let jogada = [70, 60, 20]
  let teste = (sum jogada) > Configs.numeroDeTropas

  when(teste == False) $ do
    let coronelBlotto = Strategies.getStrategy1 numeroDeTropas numeroDeCampos
    putStrLn $ "Jogador: " ++ show jogada
    putStrLn $ "Coronel: " ++ show coronelBlotto
    let vencedor = getVencedor jogada coronelBlotto
    putStrLn $ "O vencedor foi " ++ show vencedor
  -- pegarDados -- Tornar funcao repetitiva

-- |Funcao inicial
main = do
  Menus.imprimir_tela_inicial
  -- putStr "Qual é o seu nome? "
  -- nome_usuario <- getLine
  -- putStrLn $ "Olá, " ++ nome_usuario ++ ", Bem-vindo!"
  pegarDados
