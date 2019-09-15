module UI.Comp.Footer
( footer
, footerWithText
) where

import Lens.Micro ((^.))
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import Types
import Configs (concepts)

footer :: AppState -> Widget Name
footer st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 5 $
  C.center $
  strWrap $ concepts !! (st^.currentConcept)

footerWithText :: String -> Widget Name
footerWithText texto =
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str "<| Ajuda |>") $
  vLimit 5 $
  C.center $
  strWrap $ texto
