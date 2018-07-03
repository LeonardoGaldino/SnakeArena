module Obstacle where

import Definitions

type Obstacle = Position


obstacles2 :: [Obstacle]
obstacles2 = do
	[(x,y) | x <- [1..boardSize], y <- [1..boardSize], (((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)) && (x == boardSize `div` 3)) || ((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)) && (x == 2 * (boardSize `div` 3))))]


obstacles3 :: [Obstacle]
obstacles3 = do
	[(x,y) | x <- [1..boardSize], y <- [1..boardSize], (((y == boardSize `div` 3) && not((x >= boardSize `div` 4) && (x <= 3 * (boardSize `div` 4)))) || ((y == 2 * (boardSize `div` 3)) && not((x >= boardSize `div` 4) && (x <= 3 * (boardSize `div` 4)))) || ((x == (boardSize `div` 2)) && not((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)))))] 
