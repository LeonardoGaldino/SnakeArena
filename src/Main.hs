module Main where

-- Dependencies imports
import Graphics.UI.Fungen

-- Standard imports
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Char

-- Custom files imports
import Snake
import Board
import Food
import Definitions


{-
	GameLoop Thread
	keep rendering and moving snake
	until game end 
-}
gameLoop :: MVar Snake -> Food -> IO GameResult
gameLoop mSnake food = do
	snake <- takeMVar mSnake
	printBoard snake food
	let status = snakeStatus snake boardSize
	if status == HIT_WALL then
		return DEFEAT_WALL
	else if status == HIT_ITSELF then
		return DEFEAT_ITSELF
	else do
		let nextPos = nextPosition snake
		-- Eating food
		if nextPos == food then do
			let snakeTail = last $ fst snake
			let newSnake = ((fst $ moveSnake snake)++[snakeTail], snd snake)
			putMVar mSnake newSnake
			if (length (fst newSnake)) == 10 then
				return WIN
			else do
				_newFood <- newFood newSnake boardSize
				threadDelay gamePace
				gameLoop mSnake _newFood
		-- Just moving
		else do
			let movedSnake = moveSnake snake
			putMVar mSnake movedSnake
			threadDelay $ gamePace
			gameLoop mSnake food

{-
	Thread to read the user input
	(Accepts arrows keys or WASD)
	(to change movement direction)
-}
keyListenerHandler :: MVar Snake -> Char -> IO ()
keyListenerHandler mSnake pressed = do
	let direction = mapCharDirection pressed 
	snake <- takeMVar mSnake
	let newDirection = ((counterDirection snakeDirection) /= direction) ? (direction, snakeDirection)
		where snakeDirection = snd snake
	putMVar mSnake (fst snake, newDirection)
	threadDelay $ gamePace

keyListener :: MVar Snake -> IO ()
keyListener mSnake = do
	pressed <- getChar
	-- Arrow pressed
	if pressed == '\ESC' then do
		getChar -- Skip [ from the buffer ~YOLO~ KKKKK
		arrow <- getChar
		if arrow `elem` ['D', 'A', 'C', 'B'] then do
			keyListenerHandler mSnake arrow
			keyListener mSnake
		else
			keyListener mSnake
	else
		if ((toLower pressed) `elem` ['a', 'w', 'd', 's']) then do
			keyListenerHandler mSnake (toLower pressed)
			keyListener mSnake
		else
			keyListener mSnake


main :: IO ()
main = do
	hSetBuffering stdin NoBuffering -- Avoids pressing enter need to interact with game
	hSetEcho stdin False -- Avoids printing on terminal every character user input
	let newSnake = ([(1,3), (1,2), (1,1)], RIGHT)
	food <- newFood newSnake boardSize
	mSnake <- newMVar newSnake
	putStrLn "\n\n\n\nPara se movimentar: [A,S,W,D] ou [Setinhas]"
	putStrLn "Digite algo para começar."
	getChar
	forkIO $ keyListener mSnake
	gameLoop mSnake food >>= (\result -> 
		if result == WIN 
			then
				do
					putStrLn "LEVEL 2" 
					takeMVar mSnake >>= (\_ -> putMVar mSnake newSnake)
					gameLoop mSnake food >>= (\result ->  -- second level call
						if result == WIN 
							then
								do
									putStrLn "LEVEL 3" 
									--putStrLn "Digite algo para começar."
									--getChar
									takeMVar mSnake >>= (\_ -> putMVar mSnake newSnake)
									gameLoop mSnake food >>= (\result -> printGameResult result)
							else 
								printGameResult result)
			else 
				printGameResult result
		)

