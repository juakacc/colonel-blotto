{-# LANGUAGE OverloadedStrings #-}
module UI.Credits
( drawCredits
) where

import Brick
import Brick.Widgets.Core
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Data.Monoid((<>))

import Types
import UI.Comp.Header
import UI.Comp.Footer
import UI.Comp.MenuLeft
import Theme

drawCredits :: AppState -> [Widget Name]
drawCredits st = [
  vBox [ header st
       , hBox [painelEsquerdo st, body st]
       , footerWithText "Créditos" "Jogo desenvolvido como complementação para TCC.\nAluno: Joaquim\nProfessor: Edson"
       ]
  ]

body :: AppState -> Widget Name
body st =
  C.center $
  -- B.border $
  vBox [ about "Teoria dos jogos" "Falar um pouco sobre a teoria dos jogos"
       , about "Coronel Blotto" "Falar um pouco sobre o jogo em si"
       , about "Mais jogos" "Citar alguns exemplos de jogos relacionados com a teoria dos jogos"
       , about "Pesquisa" "Falar um pouco a respeito da pesquisa"
       ]
  -- hyperlink "http://www.google.com/" $ str "Teoria dos jogos, mais jogos, informações sobre o jogo, pesquisa\n"

about :: String -> String -> Widget Name
about title content =
  C.center $
  B.borderWithLabel (str $ "| " <> title <> " |") $
  strWrap content
