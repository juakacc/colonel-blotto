import Strategies
import Menus
import Configs

-- |Recupera um numero inteiro do console
getNumero = do
  l <- readLn :: IO Int
  return l

-- getNumeroDeTropas limite
--        | limite == 0 = 0
--        | n <= limite = n
--        | otherwise   = getNumeroDeTropas limite
--        where n = getNumero

-- Data para representar o vencedor em um campo
data Value = JOGADOR | CORONEL | EMPATE deriving (Show, Eq)

-- |Dado o numero de tropas do jogador e do coronel, retorna o vencedor desse campo ou um empate
getV a b
 | a == b = EMPATE
 | a > b = JOGADOR
 | otherwise = CORONEL

-- |Recebe as duas jogadas e decide quem foi o vitorioso em cada campo de batalha
getList [] [] = [EMPATE]
getList [a] [b] = [getV a b]
getList (x:xs) (y:ys) = getList [x] [y] ++ getList xs ys

-- |Recebe as duas jogadas e decide quem eh o vencedor
getVencedor jogador coronel
         | vitoriasJogador == vitoriasCoronel = EMPATE
         | vitoriasJogador > vitoriasCoronel  = JOGADOR
         | otherwise                          = CORONEL
         where vitoriasJogador = length $ filter (==JOGADOR) lista
               vitoriasCoronel = length $ filter (==CORONEL) lista
               lista = getList jogador coronel

convert :: Integral a => a -> a
convert x = x

getJogador = do
  putStr "Quantas tropas para o campo de batalha 1? "
  l1 <- getNumero
  putStr "Quantas tropas para o campo de batalha 2? "
  l2 <- getNumero
  putStr "Quantas tropas para o campo de batalha 3? "
  l3 <- getNumero
  let a = l1 :: Int
  let b = l2 :: Int
  let c = l3 :: Int
  return [a, b, c]

pegarDados = do
  -- putStr "Qual é o seu nome? "
  -- nome_usuario <- getLine
  -- putStrLn $ "Olá, " ++ nome_usuario ++ ", Bem-vindo!"

  putStr "Quantas tropas para o campo de batalha 1? "
  l1 <- getNumero
  putStr "Quantas tropas para o campo de batalha 2? "
  l2 <- getNumero
  putStr "Quantas tropas para o campo de batalha 3? "
  l3 <- getNumero
  let a = l1 :: Int
  let b = l2 :: Int
  let c = l3 :: Int

  -- Recuperar do terminal
  let jogada = [a, b, c] -- let jogada = [70, 60, 20]
  let coronelBlotto = Strategies.getStrategy1 numeroDeTropas numeroDeCampos
  putStrLn $ "Jogador: " ++ show jogada
  putStrLn $ "Coronel: " ++ show coronelBlotto

  let vencedor = getVencedor jogada coronelBlotto
  putStrLn $ "O vencedor foi " ++ show vencedor
  -- pegarDados -- Tornar funcao repetitiva

-- |Funcao inicial
main = do
  Menus.imprimir_tela_inicial
  pegarDados
