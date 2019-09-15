module App where

import qualified Graphics.Vty as V
import Brick
import Control.Monad (void)

import Types
import Theme
import UI
import Events

app :: App AppState e Name
app =
  App { appDraw = drawUI
      , appStartEvent = return
      , appHandleEvent = appEvent
      , appAttrMap = const theme
      , appChooseCursor = showFirstCursor
      }

mkInitialState =
  AppState { _uiScreen = Credits
           , _lastReportedClick = Nothing
           , _tropasRestantesJogador = 150
           , _nomeJogador = "Paulo da Silva"
           , _dicaAtual = 0
           , _field1 = 0
           , _field2 = 0
           , _field3 = 0
           }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    initialVty <- buildVty
    void $ customMain initialVty buildVty Nothing app $ mkInitialState
