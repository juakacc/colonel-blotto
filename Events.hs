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
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (&), (.~), (%~))

handleEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleEvent st (VtyEvent (V.EvResize _ _)) = continue st
handleEvent st (VtyEvent (V.EvKey V.KEsc [])) = halt st
handleEvent st e = do
    case currentScreen st of
      Initial -> handleStartEvent st e
      Play -> handlePlayEvent st e
      Results -> handleResultsEvent st e
      Credits -> handleCreditsEvent st e
      -- _ -> handleStartEvent st e
