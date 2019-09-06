module Ui
( header
, footer
, painelCoronel
) where

import Brick
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

header =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 3 $
  C.center $
  str "CORONEL BLOTTO - Teoria dos Jogos"

qtdCoronel x =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimit 4 $
  vLimit 1 $
  C.center $
  str $ show x

bot =
  vBox [C.center $ str $ "  Tropas\n" <>
                       "adversárias", C.hCenter $ qtdCoronel 40]

painelCoronel =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimitPercent 20 $
  C.center $
  vBox [C.hCenter $ str "Coronel Blotto", B.hBorder, bot]

jog =
  vBox [C.hCenter $ str $ "  Tropas\n" <>
                         "restantes", C.center $ qtdCoronel 76]

painelJogador =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimitPercent 20 $
  C.center $
  vBox [C.hCenter $ str "Jogador Joaquim",
        B.hBorder,
        jog,
        C.hCenter $ str "<JOGAR>",
        C.hCenter $ str "<LIMPAR>"]

footer =
  withBorderStyle BS.unicodeRounded $
  B.border $
  vLimit 5 $
  C.center $
  str "Frases a respeito da teoria dos jogos ao longo da interação do usuário"

form =
  withBorderStyle BS.unicodeRounded $
  B.border $
  hLimitPercent 40 $
  C.center $
  str "FORMULARIO"

ui :: Widget ()
ui =
  header
  <=> hBox [painelCoronel, form, painelJogador]
  <=> footer

main :: IO ()
main = simpleMain ui
