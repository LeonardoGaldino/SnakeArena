module Obstacles where

import Definitions

type Obstacles = [Position]


newObstacles2 :: Obstacles -> IO Obstacles
newObstacles2 obstacles = do
	[if (((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)) && (x == boardSize `div` 3)) 
	   || ((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)) && (x == 2 * (boardSize `div` 3))) then (x, y):obstacles else
	   	--else ?
	   	| x <- [1..boardSize], y <- [1..boardSize+1]]


newObstacles3 :: Obstacles -> IO Obstacles
newObstacles3 obstacles = do
	[if ((y == boardSize `div` 3) && not((x >= boardSize `div` 4) && (x <= 3 * (boardSize `div` 4))))
	   || ((y == 2 * (boardSize `div` 3)) && not((x >= boardSize `div` 4) && (x <= 3 * (boardSize `div` 4)))) 
	   || ((x == (boardSize `div` 2)) && not((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)))) then (x, y):obstacles else
	   	--else ?
	   	| x <- [1..boardSize], y <- [1..boardSize+1]] 
