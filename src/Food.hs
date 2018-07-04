module Food where

import System.Random
import Graphics.Gloss.Data.Picture

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

drawFood :: Food -> Picture
drawFood (x,y) = Translate p1 p2 (color foodColor $ rectangleSolid foodSize foodSize)
	where
		(p1, p2) = positionToPixel (x,y)