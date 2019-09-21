module App where

import qualified Graphics.Vty as V
import Brick
import Control.Monad (void)
import qualified Data.Text as T

import Events
import Types
import Theme
import UI
import UI.Comp.MkForms

app :: App AppState AppEvent Name
app =
  App { appDraw = drawUI
      , appStartEvent = return
      , appHandleEvent = handleEvent
      , appAttrMap = const theme
      , appChooseCursor = showFirstCursor
      }

mkInitialState =
  AppState { _uiScreen = Initial
           , _lastReportedClick = Nothing
           , _remainingSoldiers = 70
           , _quantitySoldiers = 150
           , _currentConcept = 0
           , _fieldsBlotto = [0, 0, 0]
           , _fields = [0, 0, 0]
           , _winner = EMPATE
           , _formFields = mkFormFields mkFormFieldsState

           , _formInfos = mkFormInfos mkFormInfosState
           , _playerName = T.pack "Anônimo"
           , _errorMsg = ""
           , _qtdFields = Little
           }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    initialVty <- buildVty
    void $ customMain initialVty buildVty Nothing app $ mkInitialState
