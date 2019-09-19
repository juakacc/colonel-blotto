module UI.Comp.Footer
( footer
, footerWithText
, footerOfError
) where

import Lens.Micro ((^.))
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import Data.Monoid ((<>))

import Configs (concepts)
import Types
import Theme

footer :: AppState -> Widget Name
footer st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 5 $
  C.center $
  strWrap $ concepts !! (st^.currentConcept)

footerWithText :: String -> String -> Widget Name
footerWithText title text =
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str $ "| " <> title <> " |") $
  vLimit 5 $
  C.center $
  strWrap $ text

footerOfError :: AppState -> Widget Name
footerOfError st =
  withDefAttr txtError $
  footerWithText "Erro" $ st^.errorMsg
