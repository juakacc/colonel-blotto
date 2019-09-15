module UI
( drawUI
) where

import Lens.Micro
import Brick

import Types
import UI.Play
import UI.Start
import UI.Credits

drawUI :: AppState -> [Widget Name]
drawUI st =
  case st^.uiScreen of
    Initial -> drawStart st
    Play    -> drawPlay st
    Credits -> drawCredits st
