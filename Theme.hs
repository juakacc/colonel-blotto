{-# LANGUAGE OverloadedStrings #-}
module Theme
( theme
, negrito
, fVerde
, bgVerde
, fAzul
, txtError
, btPlay
, btClean
, btMenu
, btStart
, btExit
, btCredits
) where

import Brick
import Brick.Forms(invalidFormInputAttr, formAttr)
import Brick.Widgets.Edit
import Graphics.Vty

negrito :: AttrName
negrito = "negrito"

fVerde :: AttrName
fVerde = "fVerde"

bgVerde :: AttrName
bgVerde = "bgVerde"

fAzul :: AttrName
fAzul = "fAzul"

txtError :: AttrName
txtError = "error"

btPlay :: AttrName
btPlay = "btPlay"

btClean :: AttrName
btClean = "btClean"

btMenu :: AttrName
btMenu = "btMenu"

btStart :: AttrName
btStart = "btStart"

btExit :: AttrName
btExit = "btExit"

btCredits :: AttrName
btCredits = "btCredits"

theme :: AttrMap
theme = attrMap defAttr
  [ (negrito, fg white `withStyle` bold)

  , (fVerde, fg green)
  -- , (fVerde, fg $ rgbColor 70 70 70)
  , (bgVerde, bg green)
  , (fAzul, fg cyan)
  , (txtError, fg red `withStyle` bold)
  -- Buttons
  , (btPlay, black `on` yellow)
  , (btClean, black `on` white)
  , (btMenu, white `on` blue)
  , (btStart, white `on` green)
  , (btExit, white `on` red)
  , (btCredits, white `on` magenta)
  -- Forms
  , (invalidFormInputAttr, white `on` red)
  , (editFocusedAttr, black `on` yellow)
  ]
