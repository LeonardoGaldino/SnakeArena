module Main where

-- Dependencies imports
-- import Graphics.UI.Fungen

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
import Obstacle
import Level

{-
	snakeMoveAction:
	Responsible for moving a snake (possibly eating a food and generating another one)
	and handling collision etc	
-}
snakeMoveAction :: Snake -> Snake -> Food -> [Obstacle] -> IO (Snake, Food, SnakeStatus)
snakeMoveAction mover enemy food obstacles = do
	let movedSnake = moveSnake mover
	let status = snakeStatus movedSnake enemy obstacles
	if status == VALID then do
		let movedHead = head $ fst movedSnake
		if movedHead == food then do
			let grownSnake = (fst movedSnake ++ [(last $ fst mover)], snd movedSnake)
			generatedFood <- newFood grownSnake enemy obstacles 
			return (grownSnake, generatedFood, VALID)
		else
			return (movedSnake, food, VALID)
	else
		return (movedSnake, food, status)

{-
	GameLoop Thread
	keep rendering and moving both snakes
	until game end 
-}
gameLoop :: MVar Snake -> Snake -> Food -> MVar Direction -> Level -> IO GameResult
gameLoop mSnake _bot food mDir (level, gameP, maxLen, obstacles) = do
	snake <- takeMVar mSnake
	printBoard snake _bot food obstacles
	forkIO $ computeDirection _bot snake food obstacles mDir -- computes BFS in another Thread
	threadDelay gameP 							-- while gameLoop sleeps
	botDir <- takeMVar mDir
	putMVar mDir botDir
	let bot = (fst _bot, botDir)
	snakeMoveAction snake bot food obstacles >>= (\(movedSnake, food2, status) -> do
			putMVar mSnake movedSnake
			if status == VALID then
				if(length (fst snake) < maxLen) then do 
					snakeMoveAction bot movedSnake food2 obstacles >>= (\(movedBot, food3, statusBot) ->
						if statusBot == VALID then
							gameLoop mSnake movedBot food3 mDir (level, gameP, maxLen, obstacles)
						else do
							printBoard movedSnake movedBot food3 obstacles
							return $ mapSnakeStatusGameResult statusBot False
						)
				else return WIN
			else do
				printBoard movedSnake bot food2 obstacles
				return $ mapSnakeStatusGameResult status True
		)

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
	threadDelay $ gamePace!!2

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
	let bot = ([(boardSize, 3), (boardSize, 2), (boardSize, 1)], RIGHT)
	food <- newFood newSnake bot []
	mSnake <- newMVar newSnake
	mDir <- newMVar RIGHT
	putStrLn "\n\n\n\nPara se movimentar: [A,S,W,D] ou [Setinhas]"
	putStrLn "Digite algo para começar."
	getChar
	forkIO $ keyListener mSnake
	gameLoop mSnake bot food mDir level1 >>= (\result -> 
		if result == WIN 
			then
				do
					putStrLn "LEVEL 2" 
					takeMVar mSnake >>= (\_ -> putMVar mSnake newSnake)
					gameLoop mSnake bot food mDir level2 >>= (\result ->  -- second level call
						if result == WIN 
							then
								do
									putStrLn "LEVEL 3" 
									takeMVar mSnake >>= (\_ -> putMVar mSnake newSnake)
									gameLoop mSnake bot food mDir level3 >>= (\result -> printGameResult result)
							else 
								printGameResult result)
			else 
				printGameResult result
		)

