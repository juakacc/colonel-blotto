{-# LANGUAGE OverloadedStrings #-}
module AMap
( aMap
) where

import Brick.AttrMap
import Brick.Util
import qualified Graphics.Vty as V

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ ("ButtonPlay", V.black `on` V.cyan)
    , ("ButtonClean", V.black `on` V.green)
    ]
