module UI.Comp.Footer
( footer
) where

import Lens.Micro ((^.))
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import Types
import Configs (dicas)

footer :: AppState -> Widget Name
footer st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 5 $
  C.center $
  strWrap $ dicas !! (st^.dicaAtual)
