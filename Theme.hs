{-# LANGUAGE OverloadedStrings #-}
module Theme
( theme
, negrito
, fVerde
, bgVerde
, bgWin
, bgLoss
, fAzul
, txtError
, btPlay
, btPlayAgain
, btClean
, btMenu
, btStart
, btExit
, btCredits
, msgResultWin
, msgResultLoss
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

bgWin :: AttrName
bgWin = "bgWin"

bgLoss :: AttrName
bgLoss = "bgLoss"

fAzul :: AttrName
fAzul = "fAzul"

txtError :: AttrName
txtError = "error"

btPlay :: AttrName
btPlay = "btPlay"

btPlayAgain :: AttrName
btPlayAgain = "btPlayAgain"

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

msgResultWin :: AttrName
msgResultWin = "msgResultWin"

msgResultLoss :: AttrName
msgResultLoss = "msgResultLoss"

theme :: AttrMap
theme = attrMap defAttr
  [ (negrito, fg white `withStyle` bold)

  , (fVerde, fg green)
  -- , (fVerde, fg $ rgbColor 70 70 70)
  , (bgVerde, bg green)

  , (bgWin, bg green)
  , (bgLoss, bg red)
  , (msgResultWin, white `on` green `withStyle` bold `withStyle` blink)
  , (msgResultLoss, white `on` red `withStyle` bold `withStyle` blink)

  , (fAzul, fg cyan)
  , (txtError, fg red `withStyle` bold)
  -- Buttons
  , (btPlay, black `on` yellow)
  , (btPlayAgain, black `on` yellow)
  , (btClean, black `on` white)
  , (btMenu, white `on` blue)
  , (btStart, white `on` green)
  , (btExit, white `on` red)
  , (btCredits, white `on` magenta)
  -- Forms
  , (invalidFormInputAttr, white `on` red)
  , (editFocusedAttr, black `on` yellow)
  ]
