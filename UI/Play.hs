{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module UI.Play
( drawPlay
) where

import UI.Comp.Header
import UI.Comp.Footer
import UI.Comp.MkButtons
import Configs
import Theme
import Types

import qualified Data.Text as T
import Lens.Micro ((^.), (&), (.~), (%~))

import Brick
import Brick.Widgets.Core
import qualified Brick.Forms as F
import qualified Brick.Main
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

drawPlay :: AppState -> [Widget Name]
drawPlay st =
  [header st
  <=>
  (C.hCenterLayer $ hBox [painelEsquerdo st, form st, painelDireito st])
  <=> footer st]

qtdCoronel :: Int -> Widget Name
qtdCoronel x =
  withDefAttr fVerde $
  withBorderStyle BS.unicodeBold $
  B.border $
  hLimit 7 $
  vLimit 1 $
  C.center $
  withDefAttr negrito $
  withDefAttr fVerde $
  str $ show x

qtdJogador :: Int -> Widget Name
qtdJogador x =
  withDefAttr fAzul $
  withBorderStyle BS.unicodeBold $
  B.border $
  hLimit 7 $
  vLimit 1 $
  C.center $
  withDefAttr negrito $
  withDefAttr fAzul $
  str $ show x

painelEsquerdo :: AppState -> Widget Name
painelEsquerdo st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 20 $
  -- C.center $
  vBox [ C.hCenter $ btnMenu st
       , B.hBorder
       , C.hCenter $ withDefAttr negrito $ str "Atalhos"
       , B.hBorder
       , C.center $ str $ "C-s -> Sair"
       ]

painelDireito :: AppState -> Widget Name
painelDireito st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 20 $
  C.center $
  vBox [C.hCenter $ (withDefAttr negrito $ str $ "Jogador:\n") <=> (strWrap $ T.unpack $ st^.playerName),
        B.hBorder,
        C.hCenter ((C.vCenter $ str "Tropas: ") <+> C.vCenter (qtdJogador $ st^.remainingSoldiers)),
        B.hBorder,
        C.hCenter $ withDefAttr negrito $ str $ "Coronel Blotto",
        B.hBorder,
        C.hCenter $
        C.hCenter ((C.vCenter $ str "Tropas: ") <+> C.vCenter (qtdCoronel $ st^.remainingSoldiers)),
        B.hBorder,
        padBottom (Pad 1) $
        C.hCenter $ btnPlay st,
        C.hCenter $ btnClean st]

form :: AppState -> Widget Name
form st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  -- hLimit 35 $
  -- setAvailableSize (50,50) $
  C.center $ F.renderForm $ st^.formFields
