{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Name(..)
  , Value(..)
  , AppState(..)
  , lastReportedClick
  , tropasRestantesJogador
  , dicaAtual
  , nomeJogador
  -- , jogadorFields
  -- , colonelFields
  ) where

import Lens.Micro.TH (makeLenses)

data Name = ButtonPlay
          | ButtonClean
          deriving (Show, Ord, Eq)

data AppState =
  AppState { _lastReportedClick :: Maybe Name
           , _tropasRestantesJogador :: Int

           -- , _jogadorFields :: [Int]
           -- , _jogadorField2 :: Int
           -- , _jogadorField3 :: Int

           -- , _colonelFields :: [Int]
           -- , _colonelField2 :: Int
           -- , _colonelField3 :: Int

           , _nomeJogador :: String

           , _dicaAtual :: Int
           }
makeLenses ''AppState

-- Data para representar o vencedor em um campo
data Value = JOGADOR | CORONEL | EMPATE deriving (Show, Eq)
