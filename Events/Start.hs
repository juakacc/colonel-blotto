module Events.Start
( handleStartEvent
) where

import Brick
import Brick.Main

import Lens.Micro ((^.), (&), (.~), (%~))

import Types

handleStartEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
handleStartEvent st e =
  case e of
    MouseDown n _ _ _ -> do
      let st' = st & lastReportedClick .~ Just n
      case n of
        ButtonExit  -> halt st'
        ButtonStart -> continue $ st' & uiScreen .~ Play
        ButtonCredits -> continue $ st' & uiScreen .~ Credits
        _           -> continue $ st'

    MouseUp _ _ _ -> continue $ st & lastReportedClick .~ Nothing
    _ -> continue st
