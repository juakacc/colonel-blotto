{-# LANGUAGE OverloadedStrings #-}
module Ui
( header
, footer
, painelCoronel
) where

import Mouse
import Types

import qualified Graphics.Vty as V
import Data.Monoid ((<>))
import Control.Monad (void)

import Brick
import qualified Brick.Main as M
import qualified Brick.AttrMap as AT
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

header =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 3 $
  C.center $
  str "CORONEL BLOTTO - Teoria dos Jogos"

titleAttr :: AT.AttrName
titleAttr = "title"

mappings :: [(AT.AttrName, V.Attr)]
mappings =
  [ (B.borderAttr, V.cyan `on` V.black)
  , (titleAttr   , fg V.cyan)]

squareQtd x =
  updateAttrMap (AT.applyAttrMappings mappings) $
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 4 $
  vLimit 1 $
  C.center $
  withAttr titleAttr $
  str $ show x

bot =
  vBox [C.center $ str $ "  Tropas\n" <>
                       "adversárias", C.hCenter $ squareQtd 40]

painelCoronel =
  withBorderStyle BS.unicodeRounded $
  B.border $
  -- hLimitPercent 20 $
  C.center $
  vBox [C.hCenter $ str "Coronel Blotto", B.hBorder, bot]

jog :: AppState -> Widget Name
jog st =
  vBox [C.hCenter $ str $ "  Tropas\n" <>
                         "restantes", C.center $ squareQtd $ st^.tropasRestantesJogador]

painelJogador :: AppState -> Widget Name
painelJogador st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  -- hLimitPercent 20 $
  C.center $
  vBox [C.hCenter $ str "Jogador Joaquim",
        B.hBorder,
        jog st,
        padBottom (Pad 1) $ C.hCenter $ btnPlay st,
        C.hCenter $ btnClean st]

footer =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 5 $
  C.center $
  str "Frases a respeito da teoria dos jogos ao longo da interação do usuário"

form =
  withBorderStyle BS.unicodeRounded $
  B.border $
  -- hLimitPercent 40 $
  C.center $
  str "FORMULARIO"

ui :: AppState -> [Widget Name]
ui st =
  [header
  <=> (C.hCenterLayer $ hBox [hLimit 20 $ painelCoronel, hLimit 60 $ form, hLimit 20 $ painelJogador st])
  <=> footer]

app :: App AppState e Name
app =
  App { appDraw = ui
      , appStartEvent = return
      , appHandleEvent = appEvent
      , appAttrMap = const aMap
      , appChooseCursor = showFirstCursor
      }

mkInitialState =
  AppState { _lastReportedClick = Nothing
           , _tropasRestantesJogador = 150
           }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    initialVty <- buildVty
    void $ M.customMain initialVty buildVty Nothing app $ mkInitialState
