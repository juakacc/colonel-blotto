{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Testes.FormTest where

import qualified Data.Text as T
import Lens.Micro ((^.))
import Lens.Micro.TH
import Data.Monoid ((<>))

import qualified Graphics.Vty as V
import Brick
import Brick.Forms
import Brick.Focus
  ( focusGetCurrent
  , focusRingCursor
  )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

data Name = NameField
          | NumberFields
          | ThreeField
          | FourField
          | FiveField

          | NumberSoldiers
          | LittleField
          | MediumField
          | VeryField
          deriving (Eq, Ord, Show)

-- | Quantidade de tropas e campos de batalha
data Quantity = Little | Medium | Very
              deriving (Show, Eq)

data InfoState =
    InfoState { _name      :: T.Text
              , _soldiers  :: Quantity
              , _fields     :: Quantity
              } deriving (Show)
makeLenses ''InfoState

mkForm :: InfoState -> Form InfoState e Name
mkForm =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 10 $ str s <+> fill ' ') <+> w
    in newForm [ label "Nome" @@=
                   editTextField name NameField (Just 1)
               , label "Nº Tropas" @@=
                   radioField soldiers [ (Little, LittleField, "100")
                                       , (Medium, MediumField, "150")
                                       , (Very, VeryField, "200")
                                       ]
               , label "Nº Campos" @@=
                   radioField fields [ (Little, ThreeField, "3")
                                     , (Medium, FourField, "4")
                                     , (Very, FiveField, "5")
                                     ]
               ]

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (E.editAttr, V.white `on` V.black)
  , (E.editFocusedAttr, V.black `on` V.yellow)
  , (invalidFormInputAttr, V.white `on` V.red)
  , (focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: Form InfoState e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
    where
        form = B.border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
        body = str $ "- Name is free-form text\n" <>
                     "- Age must be an integer (try entering an\n" <>
                     "  invalid age!)\n" <>
                     "- Handedness selects from a list of options\n" <>
                     "- The last option is a checkbox\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"

app :: App (Form InfoState e Name) e Name
app =
    App { appDraw = draw
        , appHandleEvent = \s ev ->
            case ev of
                VtyEvent (V.EvResize {})     -> continue s
                VtyEvent (V.EvKey V.KEsc [])   -> halt s
                -- VtyEvent (V.EvKey V.KEnter [])
                    -- | focusGetCurrent (formFocus s) /= Just AddressField -> halt s
                _ -> do
                    s' <- handleFormEvent ev s
                    -- continue $ setFieldValid ((formState s')^.age >= 18) AgeField s'
                    continue s'

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

        initialInfoState = InfoState { _name = ""
                                   , _soldiers = Little
                                   , _fields = Little
                                   }
        f = mkForm initialInfoState

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing app f

    putStrLn "The starting form state was:"
    print initialInfoState

    putStrLn "The final form state was:"
    print $ formState f'

    if allFieldsValid f'
       then putStrLn "The final form inputs were valid."
       else putStrLn $ "The final form had invalid inputs: " <> show (invalidFields f')
