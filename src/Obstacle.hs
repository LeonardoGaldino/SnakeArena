module Obstacle where

import Graphics.Gloss.Data.Picture

import Definitions

type Obstacle = Position

obstaclesLevel1 :: [Obstacle]
obstaclesLevel1 = []

obstaclesLevel2 :: [Obstacle]
obstaclesLevel2 = [(x,y) | x <- [1..boardSize], y <- [1..boardSize], (((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)) && (x == boardSize `div` 3)) || ((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)) && (x == 2 * (boardSize `div` 3))))]

obstaclesLevel3 :: [Obstacle]
obstaclesLevel3 = [(x,y) | x <- [1..boardSize], y <- [1..boardSize], (((y == boardSize `div` 3) && not((x >= boardSize `div` 4) && (x <= 3 * (boardSize `div` 4)))) || ((y == 2 * (boardSize `div` 3)) && not((x >= boardSize `div` 4) && (x <= 3 * (boardSize `div` 4)))) || ((x == (boardSize `div` 2)) && not((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)))))] 

obstaclesLevelWIN :: [Obstacle]
obstaclesLevelWIN = [(1,7), (2,7), (3,7), (3,8), (3,9), (2,9), (1,9), (4,8), (5,8), -- U
					 (1,11), (2,11), (3,11), (4,11), (5,11), (5,12), (5,13), (4,13), (3,13), (2,13), (1,13), (1,12), -- O
					 (1,15), (2,15), (3,15), (4,15), (5,15), (5,16), (5,17), (4,17), (3,17), (2,17), (1,17), -- U
					 (7,5), (8,5), (9,5), (10,5), (11,5), (11,6), (11,7), (10,7), (9,7), (8,7), (7,7), (11,8), (11,9), (10,9), (9,9), (8,9), (7,9), -- W
					 (7,11), (8,11), (9,11), (10,11), (11,11), (11,12), (11,13), (10,13), (9,13), (8,13), (7,13), (7,12), -- O
					 (7,15), (8,15), (9,15), (10,15), (11,15), (8,16), (9,17), (10,18), (11,19), (10,19), (9,19), (8,19), (7,19), -- N
					 (7,21), (8,21), (9,21), (11,21)] -- !

drawObstacle :: Obstacle -> Picture
drawObstacle (x,y) = Translate p1 p2 (color obstacleColor $ rectangleSolid obstacleSize obstacleSize)
	where
		(p1, p2) = positionToPixel (x,y) 

drawObstacles :: [Obstacle] -> Picture
drawObstacles obstacles = Pictures (map drawObstacle obstacles)

