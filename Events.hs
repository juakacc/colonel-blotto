module Events
( handleEvent
) where

import Types
import Configs(concepts)
import Events.Start
import Events.Play
import Events.Credits

import Brick
-- import qualified Brick.Main as M
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (&), (.~), (%~))

-- handleEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
-- handleEvent st (MouseDown n _ _ _) = M.continue $ st & lastReportedClick .~ Just n
--                                                   & remainingSoldiers .~ 50
--                                                   & currentConcept .~ if n == ButtonPlay then proximaDica (st^.currentConcept) else st^.currentConcept
-- handleEvent st (MouseUp _ _ _) = M.continue $ st & lastReportedClick .~ Nothing
-- handleEvent st (VtyEvent (V.EvMouseUp _ _ _)) = M.continue $ st & lastReportedClick .~ Nothing
-- handleEvent st (VtyEvent (V.EvKey V.KEsc [])) = M.halt st
-- handleEvent st _ = M.continue st

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent st (VtyEvent (V.EvResize _ _)) = continue st
handleEvent st (VtyEvent (V.EvKey V.KEsc [])) = halt st
handleEvent st e = do
    case currentScreen st of
      Initial -> handleStartEvent st e
      Play -> handlePlayEvent st e
      Results -> handlePlayEvent st e 
      Credits -> handleCreditsEvent st e
      -- _ -> handleStartEvent st e
-- handleEvent st _ = continue st
