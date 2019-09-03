module Main where

import Brick
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Border
import qualified Brick.Widgets.Border.Style as BS

header :: Widget ()
header =
  withBorderStyle BS.unicodeRounded $
  border $
  vLimit 3 $
  C.center $
  str "CORONEL BLOTTO - Teoria dos Jogos"

footer :: Widget ()
footer =
  withBorderStyle BS.unicodeRounded $
  border $
  vLimit 5 $
  C.center $
  str "Frases a respeito da teoria dos jogos ao longo da interação do usuário"

ui :: Widget ()
ui =
  header
  <=> footer

main :: IO ()
main = simpleMain ui
