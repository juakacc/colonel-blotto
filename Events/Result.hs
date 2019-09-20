module Events.Result
( handleResultsEvent
) where

import Brick
import Brick.Main

import Lens.Micro ((^.), (&), (.~), (%~))

import Events.Play(cleanForm, cleanFields)
import Types
import Configs(concepts)

handleResultsEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleResultsEvent st e =
  case e of
    MouseDown ButtonMenu _ _ _      -> continue $ st & uiScreen .~ Initial
    MouseDown ButtonPlayAgain _ _ _ -> continue $ st & uiScreen .~ Play
                                                     & fields .~ cleanFields st
                                                     & formFields .~ cleanForm
    MouseDown ButtonCredits _ _ _   -> continue $ st & uiScreen .~ Credits
    MouseUp _ _ _ -> continue $ st & lastReportedClick .~ Nothing
    _ -> continue st
