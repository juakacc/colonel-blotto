{-# LANGUAGE OverloadedStrings #-}
module Theme
( theme
, negrito
, fVerde
, bgVerde
, bgGreenDark
, bgGreenLight
, bgWin
, bgLoss
, bgEmp
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
, msgResultEmp
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

bgGreenDark :: AttrName
bgGreenDark = "bgGreenDark"

bgGreenLight :: AttrName
bgGreenLight = "bgGreenLight"

bgWin :: AttrName
bgWin = "bgWin"

bgLoss :: AttrName
bgLoss = "bgLoss"

bgEmp :: AttrName
bgEmp = "bgEmp"

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

msgResultEmp :: AttrName
msgResultEmp = "msgResultEmp"

theme :: AttrMap
theme = attrMap defAttr
  [ (negrito, fg branco `withStyle` bold)
  , (txtError, fg vermelho `withStyle` bold)

  , (fAzul, fg azul)
  , (fVerde, fg verdeEscuro)

  , (bgVerde, bg $ verdeEscuro)
  , (bgGreenDark, bg verdeEscuro)
  , (bgGreenLight, bg verdeClaro)

  , (bgWin, bg verdeEscuro)
  , (bgLoss, bg vermelho)
  , (bgEmp, bg azul)
  , (msgResultWin, branco `on` verdeEscuro `withStyle` bold `withStyle` blink)
  , (msgResultLoss, branco `on` vermelho `withStyle` bold `withStyle` blink)
  , (msgResultEmp, branco `on` blue `withStyle` bold `withStyle` blink)

  -- Buttons
  , (btPlay, branco `on` azul)
  , (btPlayAgain, black `on` laranja)
  , (btClean, black `on` branco)
  , (btMenu, branco `on` cinza)
  , (btStart, branco `on` verdeEscuro)
  , (btExit, branco `on` vermelho)
  , (btCredits, branco `on` roxo)
  -- Forms
  , (invalidFormInputAttr, branco `on` red)
  , (editFocusedAttr, black `on` branco)
  ]

roxo :: Color
roxo = rgbColor 106 90 205

laranja :: Color
laranja = rgbColor 255 140 0

cinza :: Color
cinza = rgbColor 128 128 128

vermelho :: Color
vermelho = rgbColor 255 0 0

azul :: Color
azul = rgbColor 0 0 255

branco :: Color
branco = rgbColor 255 255 255

verdeClaro :: Color
verdeClaro = rgbColor 34 139 34

verdeEscuro :: Color
verdeEscuro = rgbColor 0 100 0
