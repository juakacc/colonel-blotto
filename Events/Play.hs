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
    MouseDown ButtonPlay _ _ _  -> do
      let f' = st^.formFields
      let sf = F.formState f'
      if F.allFieldsValid f'
        then continue $ st & uiScreen .~ Results
                           & currentConcept .~ nextConcept(st^.currentConcept)
                           & lastReportedClick .~ Just ButtonPlay
                           & fields .~ [sf^.field1, sf^.field2, sf^.field3]
        else continue $ st & lastReportedClick .~ Just ButtonPlay
                           & errorMsg .~ "Distribuição de tropas inválida"
    MouseDown ButtonMenu _ _ _  -> continue $ st & uiScreen .~ Initial
    MouseDown ButtonClean _ _ _ -> continue $ st & fields .~ cleanFields st  -- verificar necessidade
                                                 & formFields .~ cleanForm
                                                 & lastReportedClick .~ Just ButtonClean
    MouseDown ButtonCredits _ _ _ -> continue $ st & uiScreen .~ Credits
    MouseUp _ _ _ -> continue $ st & lastReportedClick .~ Nothing
    _ -> do
      f' <- F.handleFormEvent e $ st^.formFields
      let sf = F.formState f'
      let t = sf^.field1 + sf^.field2 + sf^.field3 -- total de tropas já utilizado
      let remaining = st^.quantitySoldiers - t
      let formFinal = F.setFieldValid (remaining >= 0) Field1 (
                      F.setFieldValid (remaining >= 0) Field2 (
                      F.setFieldValid (remaining >= 0) Field3 f'
                      ))

      let stF = st & formFields .~ formFinal
                   & remainingSoldiers .~ if remaining >= 0 then remaining else 0

      if F.allFieldsValid formFinal
        then continue $ stF & errorMsg .~ ""
        else continue $ stF & errorMsg .~ "Distribuição de tropas inválida"

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
