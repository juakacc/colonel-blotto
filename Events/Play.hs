module Events.Play
( handlePlayEvent
) where

import Brick
import qualified Brick.Forms as F
import Brick.Main

import Lens.Micro ((^.), (&), (.~), (%~))

import Types
import Configs(concepts)
import UI

handlePlayEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handlePlayEvent st e =
  case e of
    MouseDown n _ _ _ -> do
      let st' = st & lastReportedClick .~ Just n
      case n of
        ButtonPlay    -> continue $ st' & currentConcept .~ nextConcept(st'^.currentConcept)
        ButtonMenu    -> continue $ st' & uiScreen .~ Initial
        ButtonClean   -> continue $ st' & fields .~ cleanFields st'  -- verificar necessidade
                                        & formFields .~ cleanForm
        ButtonCredits -> continue $ st' & uiScreen .~ Credits
        _             -> continue $ st'
    MouseUp _ _ _ -> continue $ st & lastReportedClick .~ Nothing
    _ -> do
      f' <- F.handleFormEvent e $ st^.formFields
      continue $ st & formFields .~ f'

cleanForm :: FormFields
cleanForm = mkFormFields mkFormFieldsState

-- | Próximo conceito a ser exibido, caso seja o último da lista
-- é retornado ao primeiro indice
nextConcept :: Int -> Int
nextConcept n = if n < (length concepts) - 1 then n + 1 else 0

cleanFields :: AppState -> [Int]
cleanFields st =
  case (st^.level) of
    1 -> [0, 0, 0]
    2 -> [0, 0, 0, 0]
    _ -> [0, 0, 0]
