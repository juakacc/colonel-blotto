module Events
( appEvent
) where

import Types
import Configs(dicas)

import Brick
import qualified Brick.Main as M
import qualified Graphics.Vty as V
import Lens.Micro ((^.), (&), (.~), (%~))

appEvent :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
appEvent st (MouseDown n _ _ _) = M.continue $ st & lastReportedClick .~ Just n
                                                  & tropasRestantesJogador .~ 50
                                                  & dicaAtual .~ if n == ButtonPlay then proximaDica (st^.dicaAtual) else st^.dicaAtual
appEvent st (MouseUp _ _ _) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (VtyEvent (V.EvMouseUp _ _ _)) = M.continue $ st & lastReportedClick .~ Nothing
appEvent st (VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st

proximaDica n = if n < (length dicas) - 1 then n + 1 else 0
