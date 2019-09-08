{-# LANGUAGE TemplateHaskell #-}
module Types
  ( Name(..)
  , AppState(..)
  , lastReportedClick
  ) where

import Lens.Micro.TH (makeLenses)

data Name = Button1
          | Button2
          deriving (Show, Ord, Eq)

data AppState =
  AppState { _lastReportedClick :: Maybe Name
           }
makeLenses ''AppState
