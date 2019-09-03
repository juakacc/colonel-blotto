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

data Name = CbaField
          | CbbField
          | CbcField
          deriving (Eq, Ord, Show)

data UserInfo =
    UserInfo { _cba :: Int
             , _cbb :: Int
             , _cbc :: Int
             }
             deriving (Show)
makeLenses ''UserInfo

mkForm :: UserInfo -> Form UserInfo e Name
mkForm = newForm [ B.borderWithLabel (str "Campo 01") @@=
                   editShowableField cba CbaField
                 , B.borderWithLabel (str "Campo 02") @@=
                   editShowableField cbb CbbField
                 , B.borderWithLabel (str "Campo 03") @@=
                   editShowableField cbc CbcField
                ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.green)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.blue)
  ]

-- header :: Widget ()
header =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 3 $
  C.center $
  str "CORONEL BLOTTO - Teoria dos Jogos"

-- footer :: Widget ()
footer =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 5 $
  C.center $
  str "Frases a respeito da teoria dos jogos ao longo da interação do usuário"

draw :: Form UserInfo e Name -> [Widget Name]
draw f = [header <=> C.hCenter form <=> footer]
    where
        form = B.borderWithLabel (str "Campos de batalha") $ padAll 1 $ hLimit 20 $ renderForm f
        header = withBorderStyle BS.unicodeRounded $ B.border $ vLimit 3 $ C.center $ str "CORONEL BLOTTO - Teoria dos Jogos"
        footer = withBorderStyle BS.unicodeRounded $ B.border $ vLimit 3 $ C.center $ str "Frases a respeito da teoria dos jogos ao longo da interação do usuário"

app :: App (Form UserInfo e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                _ -> do
                    s' <- handleFormEvent ev s
                    -- Incluir validações aqui
                    let total = (formState s')^.cba + (formState s')^.cbb + (formState s')^.cbc
                    continue $
                     setFieldValid (total <= numeroDeTropas) CbaField (
                     setFieldValid (total <= numeroDeTropas) CbbField (
                     setFieldValid (total <= numeroDeTropas) CbcField s'
                     ))

        , appChooseCursor = focusRingCursor formFocus
        , appStartEvent = return
        , appAttrMap = const theMap
        }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

        initialUserInfo = UserInfo { _cba = 0
                                   , _cbb = 0
                                   , _cbc = 0
                                   }
        f = mkForm initialUserInfo

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app f

    putStr "Estado inicial: "
    print initialUserInfo

    putStr "Estado final: "
    print $ formState f'

    if allFieldsValid f'
       then putStrLn "Todos os campos estão válidos."
       else putStrLn $ "Os seguintes campos estão inválidos: " <> show (invalidFields f')
