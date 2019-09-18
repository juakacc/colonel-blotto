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
    MouseDown n _ _ _ -> do
      let st' = st & lastReportedClick .~ Just n
      case n of
        ButtonExit  -> halt st'
        ButtonStart -> do
          let f' = st'^.formInfos
          let sf = F.formState f'
          if F.allFieldsValid f'
            then continue $ st' & uiScreen    .~ Play
                                & playerName  .~ (if (T.length (sf^.nameI) == 0) then (T.pack "Sem nome") else (sf^.nameI))
                                & qtdFields   .~ sf^.fieldsI
                                & qtdSoldiers .~ sf^.soldiersI
                                & quantitySoldiers .~ (getQtdSoldiers $ sf^.soldiersI)
                                & remainingSoldiers .~ (getQtdSoldiers $ sf^.soldiersI)
            else continue $ st'
        ButtonCredits -> continue $ st' & uiScreen .~ Credits
        _             -> continue $ st'

    MouseUp _ _ _ -> continue $ st & lastReportedClick .~ Nothing
    _ -> do
      f' <- F.handleFormEvent e $ st^.formInfos
      continue $ st & formInfos .~ f'

-- handleStartEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
-- handleStartEvent st e =
--   case e of
--     MouseDown n _ _ _ -> do
--       let st' = st & lastReportedClick .~ Just n
--       case n of
--         ButtonExit  -> halt st'
--         ButtonStart -> do
--           let f' = st'^.formInfos
--           let sf = F.formState f'
--           if F.allFieldsValid f'
--             then continue $ st' & uiScreen    .~ Play
--                                 & playerName  .~ (if (T.length (sf^.nameI) == 0) then (T.pack "Sem nome") else (sf^.nameI))
--                                 & qtdFields   .~ sf^.fieldsI
--                                 & qtdSoldiers .~ sf^.soldiersI
--                                 & quantitySoldiers .~ (getQtdSoldiers $ sf^.soldiersI)
--                                 & remainingSoldiers .~ (getQtdSoldiers $ sf^.soldiersI)
--             else continue $ st'
--         ButtonCredits -> continue $ st' & uiScreen .~ Credits
--         _             -> continue $ st'
--
--     MouseUp _ _ _ -> continue $ st & lastReportedClick .~ Nothing
--     _ -> do
--       f' <- F.handleFormEvent e $ st^.formInfos
--       continue $ st & formInfos .~ f'
