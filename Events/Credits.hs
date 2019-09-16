module Events.Credits
( handleCreditsEvent
) where

import Brick

import Lens.Micro ((^.), (&), (.~), (%~))

import Types

handleCreditsEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleCreditsEvent st e =
  case e of
    MouseDown n _ _ _ -> do
      let st' = st & lastReportedClick .~ Just n
      case n of
        ButtonMenu -> continue $ st' & uiScreen .~ Initial
        _          -> continue st'
    MouseUp _ _ _ -> continue $ st & lastReportedClick .~ Nothing
    _ -> continue st
