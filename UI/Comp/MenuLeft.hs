module UI.Comp.MenuLeft
( painelEsquerdo
) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Data.Monoid((<>))

import UI.Comp.MkButtons
import Types
import Theme

painelEsquerdo :: AppState -> Widget Name
painelEsquerdo st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 20 $
  C.center $
  vBox [ C.hCenter $ botoes st $ currentScreen st
       , B.hBorder
       , C.hCenter $ withDefAttr negrito $ str "Atalhos"
       , B.hBorder
       , C.center $ str $ "Sair (Ctrl-q)\n"<>
                          "Início (Ctrl-s)\n"<>
                          "Créditos (Ctrl-a)\n"<>
                          "Jogar (Ctrl-y)\n"<>
                          "Vai (Ctrl-p)\n"<>
                          "Limpar (Ctrl-l)"
       ]

botoes st Results = botoes st Play
botoes st Play = vBox [padBottom (Pad 1) $ btnMenu st, btnCredits st]
botoes st Initial = vBox [btnCredits st]
botoes st Credits = vBox [padBottom (Pad 1) $ btnMenu st, btnStart st]
