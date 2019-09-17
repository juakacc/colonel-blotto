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
  , fields
  , level
  , formFields

  , AppEvent
  , FormFields
  , FieldsState(..)
  , field1
  , field2
  , field3
  -- , jogadorFields
  -- , colonelFields
  , currentScreen
  ) where

import Brick.Forms(Form)
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


-- | Screens of application
data UIScreen = Initial | Play | Credits  deriving (Eq)

data FieldsState =
  FieldsState { _field1 :: Int
              , _field2 :: Int
              , _field3 :: Int
              } deriving (Show)
makeLenses ''FieldsState

data AppEvent = AppEvent deriving (Eq)

type FormFields = Form FieldsState AppEvent Name

data AppState =
  AppState { _uiScreen :: UIScreen
           , _lastReportedClick :: Maybe Name
           , _tropasRestantesJogador :: Int
           , _level :: Int
           , _fields :: [Int]

           , _formFields :: FormFields
           -- , _field1 :: Int
           -- , _field2 :: Int
           -- , _field3 :: Int
           -- , _jogadorFields :: [Int]
           -- , _jogadorField2 :: Int
           -- , _jogadorField3 :: Int

           -- , _colonelFields :: [Int]
           -- , _colonelField2 :: Int
           -- , _colonelField3 :: Int

           , _nomeJogador :: String

           , _currentConcept :: Int
           } -- deriving (Show)
makeLenses ''AppState

-- Data para representar o vencedor em um campo
data Value = JOGADOR | CORONEL | EMPATE deriving (Show, Eq)

currentScreen :: AppState -> UIScreen
currentScreen st = _uiScreen st
