{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Mouse
( button
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

drawUi :: AppState -> [Widget Name]
drawUi st = [button st]

-- |Função responsável por criar botões 
-- mkButton

-- button :: St -> Widget Name
button st = let wasClicked = st^.lastReportedClick == Just Button1
            in clickable Button1 $
               withDefAttr "button1" $
               B.border $
               padLeftRight (if wasClicked then 2 else 3) $
               str (if wasClicked then "<" <> "Jogar" <> ">" else "Jogar")

-- buttonLayer :: St -> Widget Name
-- buttonLayer st =
--       C.hCenterLayer (padBottom (Pad 1) $ str "Click a button:") <=>
--       C.hCenterLayer (hBox $ padLeftRight 1 <$> buttons)
--     where
--         buttons = mkButton <$> buttonData
--         buttonData = [ (Button1, "Button 1", "button1")
--                      , (Button2, "Button 2", "button2")
--                      ]
--         mkButton (name, label, attr) =
--             let wasClicked = st^.lastReportedClick == Just name
--             in clickable name $
--                withDefAttr attr $
--                B.border $
--                padLeftRight (if wasClicked then 2 else 3) $
--                str (if wasClicked then "<" <> label <> ">" else label)

appEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
appEvent st (MouseDown n _ _ _) = M.continue $ st & lastReportedClick .~ Just n
appEvent st (MouseUp _ _ _) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (VtyEvent (V.EvMouseUp _ _ _)) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ ("button1", V.black `on` V.cyan)
    , ("button2", V.black `on` V.green)
    ]

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

           }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    initialVty <- buildVty
    void $ M.customMain initialVty buildVty Nothing app $ mkInitialState
