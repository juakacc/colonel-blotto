{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Lens.Micro
import Lens.Micro.TH
import Data.Monoid ((<>))

import Lens.Micro.TH (makeLenses)

import qualified Graphics.Vty as V
import Brick
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , editShowableField
  , setFormFocus
  , (@@=)
  )
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border.Style as BS

import Configs (numeroDeTropas)
import UI.Comp.Header
import UI.Comp.Footer
import Types

data AppEvent = AppEvent deriving (Eq)
type FieldForm = Form FormState AppEvent Name

data FormState =
  FormState { _field1 :: Int
            , _field2 :: Int
            , _field3 :: Int
            } deriving (Show)
makeLenses ''FormState

data EstadoGeral =
  EstadoGeral { _formFields :: FieldForm
              , _teste :: Int
              }
makeLenses ''EstadoGeral

mkForm :: FormState -> Form FormState e Name
mkForm = newForm [ B.border @@= editShowableField field1 Field1
                 , B.border @@= editShowableField field2 Field2
                 , B.border @@= editShowableField field3 Field3
                 ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.green)
  , (invalidFormInputAttr, V.black `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.blue)
  ]

draw :: EstadoGeral -> [Widget Name]
draw f = [C.hCenter form]
    where
        form = B.borderWithLabel (str $ show (f^.teste)) $ --B.border $
               hLimit 50 $
               renderForm $ (f^.formFields)

mkState =
  FormState { _field1 = 0
            , _field2 = 0
            , _field3 = 0
            }

mkEstadoGeral =
  EstadoGeral { _formFields = mkForm mkState
              , _teste = 1}

app :: App EstadoGeral AppEvent Name
app =
    App { appDraw = draw
        , appHandleEvent = handleEvent
        -- , appChooseCursor = focusRingCursor formFocus
        , appChooseCursor = showFirstCursor
        , appStartEvent = return
        , appAttrMap = const theMap
        }

handleEvent :: EstadoGeral -> BrickEvent Name AppEvent -> EventM Name (Next EstadoGeral)
handleEvent state (VtyEvent (V.EvResize {})) = continue state
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent state ev = do
  formularioTratado <- handleFormEvent ev $ state^.formFields
  let newState = state & formFields .~ formularioTratado
  let fState = formState $ newState^.formFields
  let novo = newState & teste .~ fState^.field1
  continue novo

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialUserInfo = mkEstadoGeral
        -- f = setFormFocus Field1 $ mkForm initialUserInfo

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app initialUserInfo

    -- let st = f' & formFields

    putStrLn "Estado inicial: "
    print $ f'^.teste
    --
    -- putStr "Estado final: "
    -- print $ st

    if allFieldsValid $ f'^.formFields
       then putStrLn "Todos os campos estão válidos."
       else putStrLn $ "Os seguintes campos estão inválidos: " <> show (invalidFields $ f'^.formFields)
