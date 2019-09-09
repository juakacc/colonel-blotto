{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mouse
( btnPlay
, btnClean
, mkButton
, Name
, appEvent
, aMap
) where

import Types

import Control.Applicative ((<$>))
import Lens.Micro ((^.), (&), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import Brick
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget, ViewportType(Vertical))
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core

-- |Função responsável por criar botões
mkButton :: (AppState, Name, String, AttrName) -> Widget Name
mkButton (st, name, label, attr) =
  let wasClicked = st^.lastReportedClick == Just name
  in clickable name $
     withDefAttr attr $
     B.border $
     padLeftRight (if wasClicked then 2 else 3) $
     str (if wasClicked then "<" <> label <> ">" else label)

btnPlay :: AppState -> Widget Name
btnPlay st = mkButton (st, ButtonPlay, "Jogar", "ButtonPlay")

btnClean :: AppState -> Widget Name
btnClean st = mkButton (st, ButtonClean, "Limpar", "ButtonClean")

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ ("ButtonPlay", V.black `on` V.cyan)
    , ("ButtonClean", V.black `on` V.green)
    ]

-- ---------------- TESTES ----------------

drawUi :: AppState -> [Widget Name]
drawUi st = [btnPlay st]

appEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
appEvent st (MouseDown n _ _ _) = M.continue $ st & lastReportedClick .~ Just n
appEvent st (MouseUp _ _ _) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (VtyEvent (V.EvMouseUp _ _ _)) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st

app :: M.App AppState e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appChooseCursor = M.showFirstCursor
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
