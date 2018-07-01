module Board where

import Definitions

type Board = [[Char]]

boardSize :: Int
boardSize = 10

-- Cria um tabuleiro de tamanho (boardSize X boardSize) com caracteres '.' (vazio)
initialBoard :: Board
initialBoard = take boardSize $ repeat (take boardSize $ repeat '.')