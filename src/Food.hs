module Food where

import System.Random

import Snake
import Definitions

type Food = Position

newFood :: Snake -> IO Food
newFood snake = do
	x <- randomRIO (1, boardSize)
	y <- randomRIO (1, boardSize)
	if (x,y) `elem` (fst snake) then
		newFood snake
	else
		return (x,y)
