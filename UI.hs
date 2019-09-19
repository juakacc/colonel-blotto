module UI
( drawUI
) where

import Lens.Micro
import Brick

import Types
import UI.Play
import UI.Start
import UI.Result
import UI.Credits

-- | Load the view
drawUI :: AppState -> [Widget Name]
drawUI st =
  case st^.uiScreen of
    Initial -> drawStart st
    Play    -> drawPlay st
    Results -> drawResult st
    Credits -> drawCredits st
