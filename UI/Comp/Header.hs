module UI.Comp.Header
( header
) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import Lens.Micro ((^.), (&), (.~), (%~))

import Theme
import Types
import UI.Comp.MkButtons

header :: AppState -> Widget Name
header st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 3 $
  hBox [ C.center $
         withDefAttr negrito $
         str "CORONEL BLOTTO - Teoria dos Jogos"
       , B.vBorder
       , if st^.uiScreen == Credits then btnMenu st else btnCredits st
       ]
