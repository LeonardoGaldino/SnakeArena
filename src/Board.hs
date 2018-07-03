module Board where

import Definitions
import Snake
import Food

buildBoardString :: Snake -> Snake -> Food -> String
buildBoardString (snake, _) (bot, _) food = 
	[if y == boardSize+1 then '\n' else 
		(if (x,y) == (head snake) then 'o' else 
			(if (x,y) `elem` snake then '#' else 
				(if (x,y) == food then 'x' else 
					(if (x,y) == (head bot) then 'Q' else
						(if (x,y) `elem` bot then '$' else '.')
						)
					)
				)
			) | x <- [1..boardSize], y <- [1..boardSize+1]]

printBoard :: Snake -> Snake -> Food -> (Snake -> Snake -> Food -> String) -> IO ()
printBoard snake bot food build = putStrLn $ (build snake bot food) ++ "\n\n\n\n\n\n\n"