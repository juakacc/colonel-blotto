{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Lens.Micro ((^.))
import Lens.Micro.TH
import Data.Monoid ((<>))

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
import Ui
 ( header
 , footer
 )
import Types

data FormState =
  FormState { _field1 :: Int
            , _field2 :: Int
            , _field3 :: Int
            }

mkForm :: AppState -> Form FormState e Name
mkForm = newForm [ translateBy (Location (0,0)) @@= hLimit 10 @@= (withBorderStyle BS.ascii) @@= B.borderWithLabel (str "-") @@= editShowableField field1 Field1
                 , translateBy (Location (20,3)) @@= hLimit 10 @@= (withBorderStyle BS.ascii) @@= B.borderWithLabel (str "-") @@= editShowableField field2 Field2
                 , translateBy (Location (0,3)) @@= hLimit 10 @@= (withBorderStyle BS.ascii) @@= B.borderWithLabel (str "-") @@= editShowableField field3 Field3
                 ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.green)
  , (invalidFormInputAttr, V.black `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.blue)
  ]

draw :: AppState -> [Widget Name]
draw st = [C.hCenter form]
    where
        form = B.border $
               -- C.center $
               -- padAll 1 $
               hLimit 100 $
               renderForm $
               setFormFocus Field1 $
               mkForm $ mkState

mkState =
  FormState { _field1 = 0
            , _field2 = 0
            , _field3 = 0
            }

-- app :: App (Form AppState e Name) e Name
app :: App FormState e Name
app =
    App { appDraw = draw
        , appHandleEvent = handleEvent
        -- , appChooseCursor = focusRingCursor formFocus
        , appChooseCursor = showFirstCursor
        , appStartEvent = return
        , appAttrMap = const theMap
        }

-- -| Função para manipular os eventos gerados pelo jogo
-- handleEvent :: Form AppState e Name -> BrickEvent Name e -> EventM Name (Next (Form AppState e Name))
-- handleEvent :: Form AppState e Name -> BrickEvent Name e -> EventM Name (Next (Form AppState e Name))
handleEvent state (VtyEvent (V.EvResize {})) = continue state
handleEvent state (VtyEvent (V.EvKey V.KEsc [])) = halt state
handleEvent state event = continue state
  -- state' <- handleFormEvent event state
  -- Incluir validações aqui
  -- let total = (formState state')^.field1 + (formState state')^.field2 + (formState state')^.field3
  -- continue $
  --  setFieldValid (total <= numeroDeTropas) Field1 (
  --  setFieldValid (total <= numeroDeTropas) Field2 (
  --  setFieldValid (total <= numeroDeTropas) Field3 state'
  --  ))

mkInitialState =
 AppState { _lastReportedClick = Nothing
          , _tropasRestantesJogador = 150
          , _nomeJogador = "Paulo da Silva"
          , _dicaAtual = 0
          , _field1 = 0
          , _field2 = 0
          , _field3 = 0
          }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialUserInfo = mkInitialState
        f = mkForm initialUserInfo

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app initialUserInfo

    -- putStr "Estado inicial: "
    -- print initialUserInfo
    --
    -- putStr "Estado final: "
    print $ (initialUserInfo)^.field1

    -- if allFieldsValid f'
    --    then putStrLn "Todos os campos estão válidos."
    --    else putStrLn $ "Os seguintes campos estão inválidos: " <> show (invalidFields f')
