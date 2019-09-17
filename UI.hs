module UI
( drawUI
, mkFormFields
) where

import Lens.Micro
import Brick
import Brick.Forms
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

import Types
import UI.Play
import UI.Start
import UI.Credits

-- | Load the view
drawUI :: AppState -> [Widget Name]
drawUI st =
  case st^.uiScreen of
    Initial -> drawStart st
    Play    -> drawPlay st
    Credits -> drawCredits st



-- mkFormFieldsState :: FieldsState
mkFormFieldsState =
  FieldsState { _field1 = 0
              , _field2 = 0
              , _field3 = 0
              }

mkFormFields :: FieldsState -> Form FieldsState AppEvent Name
mkFormFields =
  newForm [ translateBy (Location (0,0)) @@= hLimit 10 @@= (withBorderStyle BS.ascii) @@= B.border @@= editShowableField field1 Field1
          , translateBy (Location (20,3)) @@= hLimit 10 @@= (withBorderStyle BS.ascii) @@= B.borderWithLabel (str "-") @@= editShowableField field2 Field2
          , translateBy (Location (0,3)) @@= hLimit 10 @@= (withBorderStyle BS.ascii) @@= B.borderWithLabel (str "-") @@= editShowableField field3 Field3
          ]
