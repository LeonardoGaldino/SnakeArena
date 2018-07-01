module Definitions where

type Position = (Int, Int)
type Snake = [Position]
type Board = [[Char]]

data Direction = LEFT | UP | RIGHT | DOWN

boardSize :: Int
boardSize = 10

-- Cria um tabuleiro de tamanho (boardSize X boardSize) com caracteres '.' (vazio)
initialBoard :: Board
initialBoard = take boardSize $ repeat (take boardSize $ repeat '.')
