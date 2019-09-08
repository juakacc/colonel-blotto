{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative ((<$>))
import Lens.Micro ((^.), (&), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget, ViewportType(Vertical))
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core

data Name = Button1 | Button2 deriving (Show, Ord, Eq)

data St =
    St { _lastReportedClick :: Maybe Name }
makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ buttonLayer st
    , str "Testando o restante"
    ]

buttonLayer :: St -> Widget Name
buttonLayer st =
    -- C.vCenterLayer $
      C.hCenterLayer (padBottom (T.Pad 1) $ str "Click a button:") <=>
      C.hCenterLayer (hBox $ padLeftRight 1 <$> buttons)
    where
        buttons = mkButton <$> buttonData
        buttonData = [ (Button1, "Button 1", "button1")
                     , (Button2, "Button 2", "button2")
                     ]
        mkButton (name, label, attr) =
            let wasClicked = st^.lastReportedClick == Just name
            in clickable name $
               withDefAttr attr $
               B.border $
               padLeftRight (if wasClicked then 2 else 3) $
               str (if wasClicked then "<" <> label <> ">" else label)

appEvent :: St -> T.BrickEvent Name e -> T.EventM Name (T.Next St)
appEvent st (T.MouseDown n _ _ _) = M.continue $ st & lastReportedClick .~ Just n
appEvent st (T.MouseUp _ _ _) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (T.VtyEvent (V.EvMouseUp _ _ _)) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ ("button1", V.black `on` V.cyan)
    , ("button2", V.black `on` V.green)
    ]

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appChooseCursor = M.showFirstCursor
          }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    initialVty <- buildVty
    void $ M.customMain initialVty buildVty Nothing app $ St Nothing
