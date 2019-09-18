module Events.Start
( handleStartEvent
) where

import Brick
import qualified Brick.Forms as F
import Brick.Main
import qualified Data.Text as T
import Lens.Micro ((^.), (&), (.~), (%~))

import Types

handleStartEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleStartEvent st e =
  case e of
    MouseDown ButtonExit _ _ _  -> halt $ st & lastReportedClick .~ Just ButtonExit
    MouseDown ButtonStart _ _ _ -> do
      let f' = st^.formInfos
      let sf = F.formState f'
      if F.allFieldsValid f'
        then continue $ st & uiScreen          .~ Play
                           & playerName        .~ (if (T.length (sf^.nameI) == 0) then (T.pack "Anônimo") else (sf^.nameI))
                           & qtdFields         .~ sf^.fieldsI
                           & quantitySoldiers  .~ (getQtdSoldiers $ sf^.soldiersI)
                           & remainingSoldiers .~ (getQtdSoldiers $ sf^.soldiersI)
                           & lastReportedClick .~ Just ButtonStart
        else continue $ st & lastReportedClick .~ Just ButtonStart
    MouseDown ButtonCredits _ _ _ -> continue $ st & uiScreen .~ Credits
    MouseUp _ _ _ -> continue $ st & lastReportedClick .~ Nothing
    _ -> do
      f' <- F.handleFormEvent e $ st^.formInfos
      continue $ st & formInfos .~ f'
