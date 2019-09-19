module UI.Result
( drawResult
) where

import Brick

import UI.Comp.Header
import UI.Comp.Footer
import UI.Comp.MenuLeft
import Types

drawResult :: AppState -> [Widget Name]
drawResult st = [
  vBox [ header st
       , hBox [painelEsquerdo st]
       , footer st
       ]
  ]