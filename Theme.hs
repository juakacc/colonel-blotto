{-# LANGUAGE OverloadedStrings #-}
module Theme
( theme
, negrito
, fVerde
, fAzul
, btPlay
, btClean
, btMenu
, btStart
, btExit
, btCredits
) where

import Brick
import Brick.Forms(invalidFormInputAttr)
import Brick.Widgets.Edit
import Graphics.Vty

negrito :: AttrName
negrito = "negrito"

fVerde :: AttrName
fVerde = "fVerde"

fAzul :: AttrName
fAzul = "fAzul"

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

-- theMap = attrMap V.defAttr
--   [ (E.editAttr, V.white `on` V.black)
--   , (focusedFormInputAttr, V.black `on` V.blue)
--   ]

theme :: AttrMap
theme = attrMap defAttr
  [ (negrito, fg white `withStyle` bold)

  , (fVerde, fg green)
  , (fAzul, fg cyan)
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
