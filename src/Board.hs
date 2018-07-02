module Board where

import Definitions
import Snake
import Food

buildBoardString :: Snake -> Food -> String
buildBoardString (snake, _) food = [if y == boardSize+1 then '\n' else (if (x,y) == (head snake) then 'o' else (if (x,y) `elem` snake then '#' else (if (x,y) == food then 'x' else '.'))) | x <- [1..boardSize], y <- [1..boardSize+1]]

printBoard :: Snake -> Food -> IO ()
printBoard snake food = putStrLn $ (buildBoardString snake food) ++ "\n\n\n\n\n\n\n"