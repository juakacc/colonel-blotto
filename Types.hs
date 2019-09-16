{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Name(..)
  , Value(..)
  , AppState(..)
  , UIScreen(..)
  , uiScreen
  , lastReportedClick
  , tropasRestantesJogador
  , currentConcept
  , nomeJogador
  , field1
  , field2
  , field3
  -- , jogadorFields
  -- , colonelFields
  , currentScreen
  ) where

import Lens.Micro.TH (makeLenses)

data Name = ButtonPlay
          | ButtonClean
          | ButtonMenu
          | ButtonStart
          | ButtonExit
          | ButtonCredits
          | Field1
          | Field2
          | Field3
          | NameField
          deriving (Show, Ord, Eq)

data UIScreen = Initial | Play | Credits  deriving (Eq)

data AppState =
  AppState { _uiScreen :: UIScreen
           , _lastReportedClick :: Maybe Name
           , _tropasRestantesJogador :: Int

           , _field1 :: Int
           , _field2 :: Int
           , _field3 :: Int
           -- , _jogadorFields :: [Int]
           -- , _jogadorField2 :: Int
           -- , _jogadorField3 :: Int

           -- , _colonelFields :: [Int]
           -- , _colonelField2 :: Int
           -- , _colonelField3 :: Int

           , _nomeJogador :: String

           , _currentConcept :: Int
           }
           -- deriving (Show)
makeLenses ''AppState

-- Data para representar o vencedor em um campo
data Value = JOGADOR | CORONEL | EMPATE deriving (Show, Eq)

currentScreen :: AppState -> UIScreen
currentScreen st = _uiScreen st
