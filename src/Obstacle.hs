module Obstacle where

import Definitions

type Obstacle = Position

obstaclesLevel1 :: [Obstacle]
obstaclesLevel1 = []

obstaclesLevel2 :: [Obstacle]
obstaclesLevel2 = [(x,y) | x <- [1..boardSize], y <- [1..boardSize], (((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)) && (x == boardSize `div` 3)) || ((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)) && (x == 2 * (boardSize `div` 3))))]

obstaclesLevel3 :: [Obstacle]
obstaclesLevel3 = [(x,y) | x <- [1..boardSize], y <- [1..boardSize], (((y == boardSize `div` 3) && not((x >= boardSize `div` 4) && (x <= 3 * (boardSize `div` 4)))) || ((y == 2 * (boardSize `div` 3)) && not((x >= boardSize `div` 4) && (x <= 3 * (boardSize `div` 4)))) || ((x == (boardSize `div` 2)) && not((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)))))] 
