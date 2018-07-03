module Board where

import Definitions
import Snake
import Food
import Obstacle

buildBoardString :: Snake -> Snake -> Food -> [Obstacle] -> String
buildBoardString (snake, _) (bot, _) food obstacles = 
	[if y == boardSize+1 then '\n' else
		(if (x,y) `elem` obstacles then '%' else 
			(if (x,y) == (head snake) then 'o' else 
				(if (x,y) `elem` snake then '#' else 
					(if (x,y) == food then 'x' else 
						(if (x,y) == (head bot) then 'Q' else
							(if (x,y) `elem` bot then '$' else '.')
							)
						)
					)
				)
		) | x <- [1..boardSize], y <- [1..boardSize+1]]

printBoard :: Snake -> Snake -> Food -> [Obstacle] -> IO ()
printBoard snake bot food obstacles = 
	putStrLn $ (buildBoardString snake bot food obstacles) ++ "\n\n\n\n\n\n\n"
