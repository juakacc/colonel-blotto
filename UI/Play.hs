{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module UI.Play
( drawPlay
) where

import UI.Comp.MkButtons
import Types
import UI.Comp.Header
import UI.Comp.Footer

import Configs
import Theme

import Lens.Micro ((^.), (&), (.~), (%~))
import Data.Monoid ((<>))

import Brick
import qualified Brick.Forms as F
import qualified Brick.Main
import Brick.Widgets.Core
-- import qualified Brick.AttrMap as AT
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

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

bot =
  vBox [C.center $ str $ "C-s -> Sair"]

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
       , bot
       ]

painelDireito :: AppState -> Widget Name
painelDireito st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 20 $
  C.center $
  vBox [C.hCenter $ (withDefAttr negrito $ str $ "Jogador:\n") <=> (strWrap $ st^.nomeJogador),
        B.hBorder,
        C.hCenter $ (C.vCenter $ str "Tropas: ") <+> (qtdJogador $ st^.tropasRestantesJogador),
        B.hBorder,
        C.hCenter $ withDefAttr negrito $ str $ "Coronel Blotto",
        B.hBorder,
        C.hCenter $
        C.hCenter $ (C.vCenter $ str "Tropas: ") <+> (qtdCoronel $ st^.tropasRestantesJogador),
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
  -- hBox
  --   [ translateBy (Location (0, 0)) $ qtdJogador $ (st^.fields) !! 0
  --   , translateBy (Location (5, 10)) $ qtdJogador $ (st^.fields) !! 1
  --   , translateBy (Location (10, 5)) $ qtdJogador $ (st^.fields) !! 2
  --   ]

drawPlay :: AppState -> [Widget Name]
drawPlay st =
  [header st
  <=>
  (C.hCenterLayer $ hBox [painelEsquerdo st, form st, painelDireito st])
  <=> footer st]
