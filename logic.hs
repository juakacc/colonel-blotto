import Strategies
import Menus

getNumero = do
  l <- getLine
  let resultado = read l :: Int
  return resultado

-- getNumeroDeTropas limite
--        | limite == 0 = 0
--        | n <= limite = n
--        | otherwise   = getNumeroDeTropas limite
--        where n = getNumero

-- Configurações do jogo
numeroDeCampos = 3 :: Integer
numeroDeTropas = 150 :: Integer

-- Recuperar do terminal
jogada = [70, 60, 20]
coronel = Strategies.getStrategy1 numeroDeTropas numeroDeCampos

-- Data para representar o vencedor em um campo
data Value = JOGADOR | CORONEL | EMPATE deriving (Show, Eq)

getV a b
 | a == b = EMPATE
 | a > b = JOGADOR
 | otherwise = CORONEL

-- Recebe as duas jogas e decide quem foi o vitorioso em cada campo de batalha
getList [] [] = [EMPATE]
getList [a] [b] = [getV a b]
getList (x:xs) (y:ys) = getList [x] [y] ++ getList xs ys

-- Recebe as duas jogadas e decide quem eh o vencedor
getVencedor jogador coronel
         | vitoriasJogador == vitoriasCoronel = EMPATE
         | vitoriasJogador > vitoriasCoronel  = JOGADOR
         | otherwise                          = CORONEL
         where vitoriasJogador = length $ filter (==JOGADOR) lista
               vitoriasCoronel = length $ filter (==CORONEL) lista
               lista = getList jogador coronel

main = pegarDados

pegarDados = do
  Menus.imprimir_tela_inicial
  putStr "Qual é o seu nome? "
  nome_usuario <- getLine
  putStrLn $ "Olá, " ++ nome_usuario ++ ", Bem-vindo!"
  putStrLn ""

  putStr "Quantas tropas para o campo de batalha 1? "
  -- let l1 = getNumero
  -- let imprimir = "" ++ l1
  -- print imprimir
  -- print $ numeroDeCampos + numeroDeTropas

  -- putStr "Quantas tropas para o campo de batalha 2? "
  -- l2 <- getNumero
  --
  -- putStr "Quantas tropas para o campo de batalha 3? "
  -- l3 <- getNumero

  -- putStrLn("CB1: " ++ l1 ++ " CB2: " ++ l2 ++ " CB3: " ++ l3)

  -- putStrLn("Total de tropas: " ++ ((cb1 :: Int) + (cb2 :: Int) + (cb3 :: Int)))
