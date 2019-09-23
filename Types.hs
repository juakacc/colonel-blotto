{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Name(..)
  , Vencedor(..)
  , AppState(..)
  , uiScreen
  , lastReportedClick
  , quantitySoldiers
  , remainingSoldiers
  , currentConcept
  , playerName
  , fields
  , fieldsBlotto
  , winner
  , qtdFields
  , errorMsg

  , currentScreen
  , getQtdSoldiers
  , getQtdFields

  , AppEvent
  , FormFields
  , formFields
  , FieldsState(..)
  , field1
  , field2
  , field3
  , field4
  , field5

  , FormInfos
  , formInfos
  , InfoState(..)
  , nameI
  , soldiersI
  , fieldsI

  , Quantity(..)
  , UIScreen(..)
  ) where

import Brick.Forms(Form)
import qualified Data.Text as T
import Lens.Micro.TH (makeLenses)

-- | Quantidade de tropas e campos de batalha
data Quantity = Little | Medium | Very deriving (Show, Eq)

-- | Names of resources used on program
data Name = ButtonPlay | ButtonClean | ButtonMenu | ButtonStart | ButtonExit | ButtonCredits | ButtonPlayAgain
          -- Form of battle
          | Field1 | Field2 | Field3 | Field4 | Field5
          -- Form initial
          | NameField
          | NumberFields
          | ThreeField | FourField | FiveField
          | NumberSoldiers
          | LittleField | MediumField | VeryField
          deriving (Show, Ord, Eq)

-- | Screens of application
data UIScreen = Initial | Play | Results | Credits deriving (Eq)

-- | Data to represent the winner of a field
data Vencedor = JOGADOR | CORONEL | EMPATE deriving (Show, Eq)

data InfoState =
    InfoState { _nameI        :: T.Text
              , _soldiersI    :: Quantity
              , _fieldsI      :: Quantity
              } deriving (Show)

-- | Verificar quando ficar variável
data FieldsState =
  FieldsState { _field1 :: Int
              , _field2 :: Int
              , _field3 :: Int
              , _field4 :: Int
              , _field5 :: Int
              } deriving (Show)

-- | Events of application
data AppEvent = AppEvent deriving (Eq)

-- | Forms of application
type FormFields = Form FieldsState AppEvent Name
type FormInfos  = Form InfoState AppEvent Name

-- | State of application
data AppState =
  AppState { _uiScreen :: UIScreen
           , _lastReportedClick :: Maybe Name

           , _formInfos :: FormInfos
           , _playerName :: T.Text
           , _qtdFields :: Quantity

           , _remainingSoldiers :: Int  -- Número de soldados restantes para utilização
           , _quantitySoldiers  :: Int  -- Número total de soldados que ele pode usar

           , _formFields :: FormFields
           , _fields :: [Int]
           , _fieldsBlotto :: [Int]
           , _winner :: Vencedor
           , _errorMsg :: String
           , _currentConcept :: Int
           } -- deriving (Show)
makeLenses ''FieldsState
makeLenses ''InfoState
makeLenses ''AppState

-- | Return the current screen to print
currentScreen :: AppState -> UIScreen
currentScreen st = _uiScreen st

getQtdSoldiers :: Quantity -> Int
getQtdSoldiers Little = 100
getQtdSoldiers Medium = 150
getQtdSoldiers Very = 200

getQtdFields :: Quantity -> Int
getQtdFields Little = 3
getQtdFields Medium = 4
getQtdFields Very = 5
