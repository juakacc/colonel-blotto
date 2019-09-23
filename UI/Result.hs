module UI.Result
( drawResult
) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Data.Text as T
import Lens.Micro((^.))

import UI.Comp.MkButtons
import UI.Comp.Header
import UI.Comp.Footer
import UI.Comp.MenuLeft
import Theme
import Types

drawResult :: AppState -> [Widget Name]
drawResult st = [
  vBox [ header st
       , hBox [painelEsquerdo st, battleFieldResult st, painelDireito st]
       , footer st
       ]
  ]

painelDireito :: AppState -> Widget Name
painelDireito st =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 20 $
  -- C.hCenter $
  vBox [ C.hCenter $ (withDefAttr negrito $ str $ "Jogador:\n") <=> (strWrap $ take 20 $ T.unpack $ st^.playerName)
       , B.hBorder
       , C.hCenter $ getWidgetWinner (st^.winner)
       , C.hCenter $ padTop (Pad 2) $ btnPlayAgain st
  ]

getWidgetWinner :: Vencedor -> Widget Name
getWidgetWinner JOGADOR = withBorderStyle BS.ascii $ B.border $ withDefAttr msgResultWin  $ padAll 1 $ strWrap "VOCÊ VENCEU"
getWidgetWinner CORONEL = withBorderStyle BS.ascii $ B.border $ withDefAttr msgResultLoss $ padAll 1 $ strWrap "VOCÊ PERDEU"
getWidgetWinner EMPATE  = withBorderStyle BS.ascii $ B.border $ withDefAttr msgResultEmp  $ padAll 1 $ strWrap "EMPATE"

battleFieldResult :: AppState -> Widget Name
battleFieldResult st =
  withBorderStyle BS.unicodeRounded $
  B.borderWithLabel (str "/ Situação final /") $
  C.center $
  case st^.qtdFields of
    Little -> vBox [ translateBy (Location (5, 0)) $ resultTitle st
                   , translateBy (Location (0,0)) $ resultBox ((st^.fields) !! 0) ((st^.fieldsBlotto) !! 0)
                   , translateBy (Location (25,0)) $ resultBox ((st^.fields) !! 1) ((st^.fieldsBlotto) !! 1)
                   , translateBy (Location (0,0)) $ resultBox ((st^.fields) !! 2) ((st^.fieldsBlotto) !! 2)
                   ]
    Medium -> vBox [ translateBy (Location (5, 0)) $ resultTitle st
                   , translateBy (Location (0,0)) $ resultBox ((st^.fields) !! 0) ((st^.fieldsBlotto) !! 0)
                   , translateBy (Location (25,0)) $ resultBox ((st^.fields) !! 1) ((st^.fieldsBlotto) !! 1)
                   , translateBy (Location (0,0)) $ resultBox ((st^.fields) !! 2) ((st^.fieldsBlotto) !! 2)
                   , translateBy (Location (25,0)) $ resultBox ((st^.fields) !! 3) ((st^.fieldsBlotto) !! 3)
                   ]
    Very   -> vBox [ translateBy (Location (5, 0)) $ resultTitle st
                   , translateBy (Location (0,0)) $ resultBox ((st^.fields) !! 0) ((st^.fieldsBlotto) !! 0)
                   , translateBy (Location (25,0)) $ resultBox ((st^.fields) !! 1) ((st^.fieldsBlotto) !! 1)
                   , translateBy (Location (0,0)) $ resultBox ((st^.fields) !! 2) ((st^.fieldsBlotto) !! 2)
                   , translateBy (Location (25,0)) $ resultBox ((st^.fields) !! 3) ((st^.fieldsBlotto) !! 3)
                   , translateBy (Location (0,0)) $ resultBox ((st^.fields) !! 4) ((st^.fieldsBlotto) !! 4)
                   ]

resultTitle :: AppState -> Widget Name
resultTitle st =
  vLimit 3 $
  withBorderStyle BS.unicodeBold $
  B.border $
    (withDefAttr bgWin $ (str $ T.unpack $ st^.playerName) <+>
    B.vBorder <+>
    (withDefAttr bgLoss $ str "Coronel Blotto"))

resultBox :: Int -> Int -> Widget Name
resultBox player colonel =
  vLimit 3 $
  withDefAttr (getTheme player colonel) $
  withBorderStyle BS.unicodeBold $
  B.border $
  (str $ show player) <+> B.vBorder <+> (str $ show colonel)

getTheme player colonel
  | player > colonel = bgWin
  | player < colonel = bgLoss
  | otherwise = bgEmp
