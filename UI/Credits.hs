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
  C.hCenter $
  -- B.border $
  vBox [ about "Teoria dos jogos" "A teoria dos jogos estuda as interações dos jogadores envolvidos a fim de escolher decisões ótimas para cada um deles, de acordo com os seus interesses particulares, ou não, é muito utilizada na área econômica, entre outras."
       , about "Coronel Blotto" "Se baseia em um jogo formado por 2 jogadores, o jogador e o seu oponente, o Coronel Blotto, em que os 2 dividem suas tropas entre os campos de batalha, a fim de liderar aquele campo, e assim, conseguindo mais lideranças, vencer o jogo. A divisão de tropas é oculta para cada jogador."
       , about "Mais jogos" "Citar alguns exemplos de jogos relacionados com a teoria dos jogos"
       , about "Pesquisa" "Falar um pouco a respeito da pesquisa"
       ]
  -- hyperlink "https://www.google.com/" $ str "Teoria dos jogos, mais jogos, informações sobre o jogo, pesquisa\n"

about :: String -> String -> Widget Name
about title content =
  C.hCenter $
  B.borderWithLabel (str $ "| " <> title <> " |") $
  strWrap content
