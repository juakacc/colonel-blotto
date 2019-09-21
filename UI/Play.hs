{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module UI.Play
( drawPlay
) where

import UI.Comp.Header
import UI.Comp.Footer
import UI.Comp.MkButtons
import UI.Comp.MenuLeft
import Configs
import Theme
import Types

import qualified Data.Text as T
import Data.Monoid((<>))
import Lens.Micro ((^.), (&), (.~), (%~))

import Brick
import qualified Brick.Forms as F
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

drawPlay :: AppState -> [Widget Name]
drawPlay st = [
  vBox [ header st
       , C.hCenterLayer $ hBox [painelEsquerdo st, battleField st, painelDireito st]
       , if length (st^.errorMsg) > 0 then footerOfError st else bottom st
       ]
  ]

bottom :: AppState -> Widget Name
bottom _ =
  footerWithText "Instruções" $ "Distribua suas tropas entre os campos de batalha, " <>
                 "da melhor maneira possível, a fim de derrotar seu adversário, o " <>
                 "Coronel Blotto. No painel direito é exibido a quantidade de tropas " <>
                 "que ainda estão disponíveis.\nApós preparar suas tropas clique em Vai."

qtdCoronel :: Int -> Widget Name
qtdCoronel x =
  withDefAttr fVerde $
  withBorderStyle BS.unicode $
  B.border $
  hLimit 7 $
  vLimit 1 $
  C.center $
  withDefAttr fVerde $
  str $ show x

qtdJogador :: AppState -> Widget Name
qtdJogador st =
  withDefAttr (if length (st^.errorMsg) > 0 then txtError else fAzul) $
  withBorderStyle BS.unicode $
  B.border $
  hLimit 7 $
  vLimit 1 $
  C.center $
  withDefAttr (if length (st^.errorMsg) > 0 then txtError else fAzul) $
  str $ show $ st^.remainingSoldiers

painelDireito :: AppState -> Widget Name
painelDireito st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 20 $
  C.center $
  vBox [ C.hCenter $ (withDefAttr negrito $ str $ "Jogador:\n") <=> (strWrap $ take 20 $ T.unpack $ st^.playerName)
       , B.hBorder
       , C.hCenter ((C.vCenter $ str "Tropas: ") <+> C.vCenter (qtdJogador st))
       , B.hBorder
       , C.hCenter $ withDefAttr negrito $ str $ "Coronel Blotto"
       , B.hBorder
       , C.hCenter $ C.hCenter ((C.vCenter $ str "Tropas: ") <+> C.vCenter (qtdCoronel $ st^.quantitySoldiers))
       , B.hBorder
       , padBottom (Pad 1) $ C.hCenter $ btnPlay st
       , C.hCenter $ btnClean st
       ]

battleField :: AppState -> Widget Name
battleField st =
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str "/ Campos de batalha /") $
  C.center $
  F.renderForm $ st^.formFields
