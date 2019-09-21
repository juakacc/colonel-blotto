module UI.Comp.Header
( header
) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import Theme
import Types

header :: AppState -> Widget Name
header st =
  withDefAttr fVerde $
  withBorderStyle BS.unicodeBold $
  B.border $
  vLimit 3 $
  hBox [ C.center $
         withDefAttr negrito $
         str "CORONEL BLOTTO - Teoria dos Jogos"
       ]
