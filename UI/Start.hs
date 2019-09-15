module UI.Start
( drawStart
) where

import Brick

import Types

drawStart :: AppState -> [Widget Name]
drawStart st = [str "Bem vindo"]
