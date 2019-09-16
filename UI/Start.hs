module UI.Start
( drawStart
) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import Data.Monoid ((<>))

import Types
import UI.Comp.MkButtons
import UI.Comp.Header
import UI.Comp.Footer

drawStart :: AppState -> [Widget Name]
drawStart st = [header st <=> (btnStart st <+> B.vBorder <+> btnExit st) <=> footerS]

footerS :: Widget Name
footerS =
  footerWithText "Ajuda" $ "Preparado para jogar Coronel Blotto? Adiante temos uma " <>
                           "implementação simples dele, é um jogo muito utilizado na Teoria dos Jogos. " <>
                           "Essa é a área de ajuda, ao longo do jogo serão apresentadas dicas e conceitos sobre o assunto."
