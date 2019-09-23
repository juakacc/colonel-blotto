module UI.Start
( drawStart
) where

import Brick
import Brick.Forms(renderForm)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import Data.Monoid ((<>))
import Lens.Micro((^.))

import Types
import UI.Comp.MkButtons
import UI.Comp.Header
import UI.Comp.Footer
import UI.Comp.MenuLeft

drawStart :: AppState -> [Widget Name]
drawStart st = [
  vBox [ header st
       , hBox [painelEsquerdo st, painelCentral st]
       , footerS
       ]
  ]

painelCentral :: AppState -> Widget Name
painelCentral st =
  C.center $
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str "| Informações básicas |") $
  vBox [ form st
       , botoes st
       ]

form :: AppState -> Widget Name
form st = C.hCenter $ B.border $ renderForm $ st^.formInfos

botoes st =
  C.hCenter $
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 5 $
  hBox [ padLeftRight 1 $ btnStart st
       , padLeftRight 1 $ btnExit st
       ]

footerS :: Widget Name
footerS =
  footerWithText "Ajuda" $ "Preparado para jogar Coronel Blotto? Adiante temos uma " <>
                           "implementação simples dele, é um jogo muito utilizado na Teoria dos Jogos. " <>
                           "Essa é a área de ajuda, ao longo do jogo serão apresentadas dicas e conceitos sobre o assunto.\n" <>
                           "\n-> | Tamanho recomendado para terminal 90x40 |"
