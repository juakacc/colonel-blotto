module UI.Result
( drawResult
) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

import UI.Comp.Header
import UI.Comp.Footer
import UI.Comp.MenuLeft
import Theme
import Types

drawResult :: AppState -> [Widget Name]
drawResult st = [
      translateBy (Location (0,0)) $ resultBox 10 20
      , translateBy (Location (0,20)) $ resultBox 30 40
      , translateBy (Location (50,40)) $ resultBox 30 40
      , header st
     , painelEsquerdo st
     , footer st
  ]

battleFieldResult :: AppState -> [Widget Name]
battleFieldResult st =
  -- withBorderStyle BS.unicodeRounded $
  -- B.borderWithLabel (str "/ Campos de batalha: Situação final /") $
  -- C.center $
  -- hLimit 50 $
  -- vBox
  [ translateBy (Location (0,0)) $ resultBox 10 20
       , translateBy (Location (0,20)) $ resultBox 30 40
       , translateBy (Location (20,0)) $ resultBox 30 40
       ]

resultBox :: Int -> Int -> Widget Name
resultBox player colonel =
  vLimit 5 $
  withDefAttr bgVerde $
  withBorderStyle BS.unicodeBold $
  B.border $
  withBorderStyle BS.ascii $
  B.border $
  (str $ show player) <+> B.vBorder <+> (str $ show colonel)
