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
                 "que você ainda pode utilizar. Após preparar suas tropas clique em Jogar."

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

painelDireito :: AppState -> Widget Name
painelDireito st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 20 $
  C.center $
  vBox [ C.hCenter $ (withDefAttr negrito $ str $ "Jogador:\n") <=> (strWrap $ take 20 $ T.unpack $ st^.playerName)
       , B.hBorder
       , C.hCenter ((C.vCenter $ str "Tropas: ") <+> C.vCenter (qtdJogador $ st^.remainingSoldiers))
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
  vBox [ F.renderForm $ st^.formFields
       ]
  -- hLimit 35 $
  -- setAvailableSize (50,50) $
