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
       , footerWithText "Créditos" $ "Jogo desenvolvido como parte do projeto de conclusão " <>
                                     "de curso de Computação/UEPB. Desenvolvido em 2019. Desenvolvido " <>
                                     "em Haskell, utilizando Brick para desenvolvimento da GUI. Segue a licença GPL :)\n" <>
                                     "Orientador: Edson Holanda Cavalcante Junior\n" <>
                                     "Aluno: Joaquim Aníbal Rocha Neto"
       ]
  ]

body :: AppState -> Widget Name
body st =
  C.center $
  vBox [ about "Teoria dos jogos" "Veja mais em -> https://is.gd/RAuJAa"
       , about "Coronel Blotto" "Veja mais em -> https://is.gd/05SNDN"
       , about "Mais jogos usados na Teoria dos jogos" "Veja mais em ->  https://is.gd/jHsWnV"
       , about "Pesquisa" "Veja mais em -> https://is.gd/1uUx8h"
       ]

about :: String -> String -> Widget Name
about title content =
  C.hCenter $
  B.borderWithLabel (str $ "| " <> title <> " |") $
  strWrap content
