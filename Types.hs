{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Name(..)
  , AppState(..)
  , lastReportedClick
  , tropasRestantesJogador
  ) where

import Lens.Micro.TH (makeLenses)

data Name = ButtonPlay
          | ButtonClean
          deriving (Show, Ord, Eq)

data AppState =
  AppState { _lastReportedClick :: Maybe Name
           , _tropasRestantesJogador :: Int
           }
makeLenses ''AppState
