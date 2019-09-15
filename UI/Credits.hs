module UI.Credits
( drawCredits
) where

import Brick

import Types
import UI.Comp.Header
import UI.Comp.Footer

drawCredits :: AppState -> [Widget Name]
drawCredits st = [header st <=> footer st]
