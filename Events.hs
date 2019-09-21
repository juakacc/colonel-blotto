module Events
( handleEvent
) where

import Types
import Configs(concepts)
import Events.Start
import Events.Play
import Events.Result
import Events.Credits

import Brick
import qualified Brick.Forms as F
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (&), (.~))

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent st (VtyEvent (V.EvResize _ _)) = continue st
handleEvent st (VtyEvent (V.EvKey V.KEsc [])) = halt st
handleEvent st (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt st
handleEvent st (VtyEvent (V.EvKey (V.KChar 'a') [V.MCtrl])) = continue $ st & uiScreen .~ Credits
handleEvent st (VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl])) = continue $ cleanFields st
handleEvent st (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) = continue $ st & uiScreen .~ Initial
handleEvent st (VtyEvent (V.EvKey (V.KChar 'y') [V.MCtrl])) = continue $ cleanFields st & uiScreen .~ Play
handleEvent st (VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl])) = do
  if st^.uiScreen == Play
    then continue $ playEvent st & lastReportedClick .~ Nothing
    else continue st
handleEvent st e = do
    case currentScreen st of
      Initial -> handleStartEvent st e
      Play -> handlePlayEvent st e
      Results -> handleResultsEvent st e
      Credits -> handleCreditsEvent st e
      -- _ -> handleStartEvent st e
