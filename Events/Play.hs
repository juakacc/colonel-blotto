module Events.Play
( handlePlayEvent
) where

import Brick
import qualified Brick.Forms as F
import Brick.Main

import Lens.Micro ((^.), (&), (.~), (%~))

import Types
import Configs(concepts)
import UI.Comp.MkForms(mkFormFields, mkFormFieldsState)

handlePlayEvent :: AppState -> BrickEvent Name AppEvent -> EventM Name (Next AppState)
handlePlayEvent st e =
  case e of
    MouseDown ButtonPlay _ _ _  -> continue $ st & currentConcept .~ nextConcept(st^.currentConcept)
                                                 & lastReportedClick .~ Just ButtonPlay
    MouseDown ButtonMenu _ _ _  -> continue $ st & uiScreen .~ Initial
    MouseDown ButtonClean _ _ _ -> continue $ st & fields .~ cleanFields st  -- verificar necessidade
                                                 & formFields .~ cleanForm
                                                 & lastReportedClick .~ Just ButtonClean
    MouseDown ButtonCredits _ _ _ -> continue $ st & uiScreen .~ Credits
    MouseUp _ _ _ -> continue $ st & lastReportedClick .~ Nothing
    _ -> do
      f' <- F.handleFormEvent e $ st^.formFields
      continue $ st & formFields .~ f'

-- | Clean the fields of forms with battle fields, creating a new state formFields
cleanForm :: FormFields
cleanForm = mkFormFields mkFormFieldsState

-- | Próximo conceito a ser exibido, caso seja o último da lista
-- é retornado ao primeiro indice
nextConcept :: Int -> Int
nextConcept n = if n < (length concepts) - 1 then n + 1 else 0

cleanFields :: AppState -> [Int]
cleanFields st =
  case (st^.qtdFields) of
    Little -> [0, 0, 0]
    Medium -> [0, 0, 0, 0]
    _ -> [0, 0, 0, 0]
