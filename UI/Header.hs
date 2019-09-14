module UI.Header
( header
) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import Theme

header =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 3 $
  C.center $
  withDefAttr negrito $
  str "CORONEL BLOTTO - Teoria dos Jogos"
