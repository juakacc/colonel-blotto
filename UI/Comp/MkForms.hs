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

import qualified Data.Text as T

import Types
import Theme

mkFormFieldsState :: FieldsState
mkFormFieldsState =
  FieldsState { _field1 = 0
              , _field2 = 0
              , _field3 = 0
              }

mkFormFields :: FieldsState -> Form FieldsState AppEvent Name
mkFormFields =
  newForm [ translateBy (Location (0,0)) @@= hLimit 10 @@= (withDefAttr bgVerde) @@= (withBorderStyle BS.ascii) @@= (B.border) @@= (withBorderStyle BS.ascii) @@= B.border @@= editShowableField field1 Field1
          , translateBy (Location (20,0)) @@= hLimit 10 @@= (withDefAttr bgVerde) @@= (withBorderStyle BS.unicodeBold) @@= (B.border) @@= (withBorderStyle BS.ascii) @@= B.borderWithLabel (str "-") @@= editShowableField field2 Field2
          , translateBy (Location (0,0)) @@= hLimit 10 @@= (withDefAttr bgVerde) @@= (withBorderStyle BS.unicodeBold) @@= (B.border) @@= (withBorderStyle BS.ascii) @@= B.borderWithLabel (str "-") @@= editShowableField field3 Field3

          -- , translateBy (Location (20,2)) @@= hLimit 10 @@= (withBorderStyle BS.unicodeBold) @@= (B.border) @@= (withBorderStyle BS.ascii) @@= B.borderWithLabel (str "-") @@= editShowableField field2 Field2
          -- , translateBy (Location (0,2)) @@= hLimit 10 @@= (withBorderStyle BS.unicodeBold) @@= (B.border) @@= (withBorderStyle BS.ascii) @@= B.borderWithLabel (str "-") @@= editShowableField field3 Field3
          ]

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
