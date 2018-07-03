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

buildBoardLvl2 :: Snake -> Snake -> Food -> String
buildBoardLvl2 (snake, _) (bot, _) food =
	[if y == boardSize+1 then '\n' else 
		(if (y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)) && (x == boardSize `div` 3) then '%' else
			(if (y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3)) && (x == 2 * (boardSize `div` 3)) then '%' else
				(if (x,y) == (head snake) then 'o' else 
			        (if (x,y) `elem` snake then '#' else 
						(if (x,y) == food then 'x' else 
							(if (x,y) == (head bot) then 'Q' else
								(if (x,y) `elem` bot then '$' else '.')
							)
						)
					)
				)
			)
		)
		| x <- [1..boardSize], y <- [1..boardSize+1]]

buildBoardLvl3 :: Snake -> Snake -> Food -> String
buildBoardLvl3 (snake, _) (bot, _) food =
	[if y == boardSize+1 then '\n' else 
		(if (y == boardSize `div` 3) && not((x >= boardSize `div` 4) && (x <= 3 * (boardSize `div` 4))) then '%' else
			(if (y == 2 * (boardSize `div` 3)) && not((x >= boardSize `div` 4) && (x <= 3 * (boardSize `div` 4))) then '%' else
				(if (x == (boardSize `div` 2)) && not((y >= boardSize `div` 3) && (y <= 2 * (boardSize `div` 3))) then '%' else
					(if (x,y) == (head snake) then 'o' else 
				        (if (x,y) `elem` snake then '#' else 
							(if (x,y) == food then 'x' else 
								(if (x,y) == (head bot) then 'Q' else
									(if (x,y) `elem` bot then '$' else '.')
								)
							)
						)
					)
				)
			)
		)
		| x <- [1..boardSize], y <- [1..boardSize+1]]

printBoard :: Snake -> Snake -> Food -> (Snake -> Snake -> Food -> String) -> IO ()
printBoard snake bot food build = putStrLn $ (build snake bot food) ++ "\n\n\n\n\n\n\n"