{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Ui
( header
, footer
, painelCoronel
) where

import MkWidgets
import Types
import Events
import AMap
import UI.Header
import UI.Footer

import Configs

import qualified Graphics.Vty as V
import Lens.Micro ((^.), (&), (.~), (%~))
import Data.Monoid ((<>))
import Control.Monad (void)

import Brick
import qualified Brick.Main as M
import qualified Brick.AttrMap as AT
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

titleAttr :: AT.AttrName
titleAttr = "title"

mappings :: [(AT.AttrName, V.Attr)]
mappings =
  [ (B.borderAttr, fg V.cyan)
  , (titleAttr   , fg V.cyan)]

squareQtd :: Int -> Widget Name
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
  hLimit 20 $
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
  hLimit 20 $
  C.center $
  vBox [C.hCenter $ str $ "Jogador\n" <> st^.nomeJogador,
        B.hBorder,
        jog st,
        B.hBorder,
        C.hCenter $ str $ "Coronel Blotto",
        B.hBorder,
        str $ "Tropas adversárias",
        C.center $ squareQtd $ st^.tropasRestantesJogador,
        B.hBorder,
        padBottom (Pad 1) $ C.hCenter $ btnPlay st,
        C.hCenter $ btnClean st]

form :: AppState -> Widget Name
form st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  -- hLimit 35 $
  -- setAvailableSize (50,50) $
  C.center $
  hBox
    [ translateBy (Location (0, 0)) $ squareQtd 0
    , translateBy (Location (5, 10)) $ squareQtd 15
    , translateBy (Location (10, 5)) $ squareQtd 10
    ]

ui :: AppState -> [Widget Name]
ui st =
  [header
  <=>
  (C.hCenterLayer $ hBox [painelCoronel, form st, painelJogador st])
  <=> footer st]

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
           , _nomeJogador = "Paulo da Silva"
           , _dicaAtual = 0
           }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    initialVty <- buildVty
    void $ M.customMain initialVty buildVty Nothing app $ mkInitialState
