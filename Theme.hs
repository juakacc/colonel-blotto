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

theme :: AttrMap
theme = attrMap defAttr
  [ (negrito, fg white `withStyle` bold)

  , (fVerde, fg green)
  , (fAzul, fg cyan)

  , (btPlay, black `on` yellow)
  , (btClean, black `on` white)
  , (btMenu, white `on` blue)
  , (btStart, white `on` green)
  , (btExit, black `on` red)
  , (btCredits, white `on` magenta)
  ]
