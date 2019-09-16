module UI.Credits
( drawCredits
) where

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C

import Types
import UI.Comp.Header
import UI.Comp.Footer

drawCredits :: AppState -> [Widget Name]
drawCredits st = [header st <=> body st <=> footer st]  -- verificar a necessidade do footer

body :: AppState -> Widget Name
body st = C.center $ B.border $ str "Teoria dos jogos, mais jogos, informações sobre o jogo, pesquisa\n"
