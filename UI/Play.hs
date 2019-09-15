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
import qualified Brick.Main
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
  vBox [C.center $ str $ "C-c"]

painelEsquerdo :: AppState -> Widget Name
painelEsquerdo st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 20 $
  -- C.center $
  vBox [ B.hBorder
       , btnCredits st
       , B.hBorder
       , C.hCenter $ withDefAttr negrito $ str "Atalhos"
       , B.hBorder
       , bot
       ]

jog :: AppState -> Widget Name
jog st =
  vBox [C.hCenter $ str $ " Tropas\n" <>
                          "restantes", C.center $ qtdJogador $ st^.tropasRestantesJogador]

painelDireito :: AppState -> Widget Name
painelDireito st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 20 $
  C.center $
  vBox [C.hCenter $ btnMenu st,
        B.hBorder,
        C.hCenter $ (withDefAttr negrito $ str $ "Jogador:\n") <=> (strWrap $ st^.nomeJogador),
        B.hBorder,
        jog st,
        B.hBorder,
        C.hCenter $ withDefAttr negrito $ str $ "Coronel Blotto",
        B.hBorder,
        C.hCenter $
        str $ "  Tropas\n"<>
              "adversárias",
        C.center $ qtdCoronel $ st^.tropasRestantesJogador,
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
  C.center $
  hBox
    [ translateBy (Location (0, 0)) $ qtdJogador 0
    , translateBy (Location (5, 10)) $ qtdJogador 15
    , translateBy (Location (10, 5)) $ qtdJogador 10
    ]

drawPlay :: AppState -> [Widget Name]
drawPlay st =
  [header st
  <=>
  (C.hCenterLayer $ hBox [painelEsquerdo st, form st, painelDireito st])
  <=> footer st]
