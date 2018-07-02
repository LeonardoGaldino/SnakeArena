module Food where

import System.Random

import Snake

type Food = (Int, Int)

newFood :: Snake -> Int -> IO Food
newFood snake boardSize = do
	x <- randomRIO (1, boardSize)
	y <- randomRIO (1, boardSize)
	if (x,y) `elem` (fst snake) then
		newFood snake boardSize
	else
		return (x,y)
