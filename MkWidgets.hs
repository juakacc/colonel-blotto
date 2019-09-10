{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module MkWidgets
( btnPlay
, btnClean
) where

import Types
import Configs

import Control.Applicative ((<$>))
import Lens.Micro ((^.))

import Brick
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B

-- |Função responsável por criar botões
mkButton :: (AppState, Name, String, AttrName) -> Widget Name
mkButton (st, name, label, attr) =
  let wasClicked = st^.lastReportedClick == Just name
  in clickable name $
     withDefAttr attr $
     B.border $
     padLeftRight (if wasClicked then 2 else 3) $
     str (if wasClicked then "<" <> label <> ">" else label)

btnPlay :: AppState -> Widget Name
btnPlay st = mkButton (st, ButtonPlay, "Jogar", "ButtonPlay")

btnClean :: AppState -> Widget Name
btnClean st = mkButton (st, ButtonClean, "Limpar", "ButtonClean")
