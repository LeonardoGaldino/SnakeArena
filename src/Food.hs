module Food where

import System.Random

import Snake
import Definitions

type Food = Position

newFood :: Snake -> Snake -> IO Food
newFood snake bot = do
	x <- randomRIO (1, boardSize)
	y <- randomRIO (1, boardSize)
	if (x,y) `elem` (fst snake) || (x,y) `elem` (fst bot) then
		newFood snake bot
	else
		return (x,y)
