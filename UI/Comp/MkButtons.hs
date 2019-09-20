{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module UI.Comp.MkButtons
( btnPlay
, btnPlayAgain
, btnClean
, btnMenu
, btnStart
, btnExit
, btnCredits
) where

import Types
import Configs
import qualified Theme as T

import Control.Applicative ((<$>))
import Lens.Micro ((^.))
import Data.Monoid ((<>))

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
btnPlay st = mkButton (st, ButtonPlay, "Vai", T.btPlay)

btnPlayAgain :: AppState -> Widget Name
btnPlayAgain st = mkButton (st, ButtonPlayAgain, "  Jogar\n" <>
                                                 "novamente", T.btPlayAgain)

btnClean :: AppState -> Widget Name
btnClean st = mkButton (st, ButtonClean, "Limpar", T.btClean)

btnMenu :: AppState -> Widget Name
btnMenu st = mkButton (st, ButtonMenu, "Menu", T.btMenu)

btnStart :: AppState -> Widget Name
btnStart st = mkButton (st, ButtonStart, "Jogar", T.btStart)

btnExit :: AppState -> Widget Name
btnExit st = mkButton (st, ButtonExit, "Sair", T.btExit)

btnCredits :: AppState -> Widget Name
btnCredits st = mkButton (st, ButtonCredits, "Sobre", T.btCredits)
