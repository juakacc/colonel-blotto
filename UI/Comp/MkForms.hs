{-# LANGUAGE OverloadedStrings #-}
module UI.Comp.MkForms
( mkFormFieldsState
, mkFormFields
, mkFormInfos
, mkFormInfosState
) where

import Brick
import Brick.Forms
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import Lens.Micro((^.))
import qualified Data.Text as T

import Types
import Theme

mkFormFieldsState :: FieldsState
mkFormFieldsState =
  FieldsState { _field1 = 0
              , _field2 = 0
              , _field3 = 0
              , _field4 = 0
              , _field5 = 0
              }

mk3 :: FieldsState -> Form FieldsState AppEvent Name
mk3 =
  newForm [ translateBy (Location (0,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field1 Field1

          , translateBy (Location (20,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field2 Field2

          , translateBy (Location (0,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field3 Field3
          ]

mk4 :: FieldsState -> Form FieldsState AppEvent Name
mk4 =
  newForm [ translateBy (Location (0,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field1 Field1

          , translateBy (Location (20,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field2 Field2

          , translateBy (Location (0,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field3 Field3

          , translateBy (Location (20,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field4 Field4
          ]

mk5 :: FieldsState -> Form FieldsState AppEvent Name
mk5 =
  newForm [ translateBy (Location (0,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field1 Field1

          , translateBy (Location (20,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field2 Field2

          , translateBy (Location (0,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field3 Field3

          , translateBy (Location (20,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field4 Field4

          , translateBy (Location (0,0)) @@= hLimit 10 @@=
            (withDefAttr bgGreenDark) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            (withDefAttr bgGreenLight) @@= (withBorderStyle BS.ascii) @@= B.border @@=
            editShowableField field5 Field5
          ]
mkFormFields :: FieldsState -> Quantity -> Form FieldsState AppEvent Name
mkFormFields fs Little = mk3 fs
mkFormFields fs Medium = mk4 fs
mkFormFields fs Very = mk5 fs


mkFormInfosState :: InfoState
mkFormInfosState =
    InfoState { _nameI     = T.pack ""
              , _soldiersI = Little
              , _fieldsI   = Little
              }

mkFormInfos :: InfoState -> Form InfoState e Name
mkFormInfos =
    let label s w = padBottom (Pad 1) $
                    (vLimit 1 $ hLimit 10 $ str s <+> fill ' ') <+> w
    in newForm [ label "Nome" @@=
                   editTextField nameI NameField (Just 1)
               , label "Nº Tropas" @@=
                   radioField soldiersI [ (Little, LittleField, "100")
                                       , (Medium, MediumField, "150")
                                       , (Very, VeryField, "200")
                                       ]
               , label "Nº Campos" @@=
                   radioField fieldsI [ (Little, ThreeField, "3")
                                     , (Medium, FourField, "4")
                                     , (Very, FiveField, "5")
                                     ]
               ]
