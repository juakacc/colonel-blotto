module Events.Start
( handleStartEvent
) where

import Brick
import qualified Brick.Forms as F
import Brick.Main
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (&), (.~), (%~))

import Events.Play(cleanFields)
import Types

handleStartEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handleStartEvent st e =
  case e of
    MouseDown ButtonExit _ _ _  -> halt $ st & lastReportedClick .~ Just ButtonExit
    VtyEvent (V.EvKey V.KEnter []) -> continue $ startEvent st
    MouseDown ButtonStart _ _ _ -> continue $ startEvent st
    MouseDown ButtonCredits _ _ _ -> continue $ st & uiScreen .~ Credits
    MouseUp _ _ _ -> continue $ st & lastReportedClick .~ Nothing
    _ -> do
      f' <- F.handleFormEvent e $ st^.formInfos
      continue $ st & formInfos .~ f'

startEvent :: AppState -> AppState
startEvent st = do
  let f' = st^.formInfos
  let sf = F.formState f'
  if F.allFieldsValid f'
    then do
      let st' = st & uiScreen          .~ Play
                   & playerName        .~ (if (T.length (sf^.nameI) == 0) then (T.pack "Anônimo") else (sf^.nameI))
                   & qtdFields         .~ sf^.fieldsI
                   & quantitySoldiers  .~ (getQtdSoldiers $ sf^.soldiersI)
                   & remainingSoldiers .~ (getQtdSoldiers $ sf^.soldiersI)
                   & lastReportedClick .~ Just ButtonStart
      cleanFields st'
    else st & lastReportedClick .~ Just ButtonStart
