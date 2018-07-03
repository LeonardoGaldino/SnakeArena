module Food where

import System.Random

import Snake
import Definitions
import Obstacle

type Food = Position

newFood :: Snake -> Snake -> [Obstacle] -> IO Food
newFood snake bot obstacles = do
	x <- randomRIO (1, boardSize)
	y <- randomRIO (1, boardSize)
	if (x,y) `elem` (fst snake) || (x,y) `elem` (fst bot) || (x,y) `elem` obstacles then
		newFood snake bot obstacles
	else
		return (x,y)
