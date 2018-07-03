module Main where

-- Dependencies imports
import Control.ThreadPool (threadPoolIO) -- threadPool for BFS computations

-- Standard imports
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan -- channels for threadPool
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
type ChanBFS = Chan (Snake, Snake, Position, [Obstacle])

gameLoop :: MVar Snake -> Snake -> Food -> Level -> (ChanBFS, Chan Direction) -> IO GameResult
gameLoop mSnake _bot food (level, gameP, maxLenP, maxLenB, obstacles) (chanIn, chanOut) = do
	snake <- takeMVar mSnake
	printBoard snake _bot food obstacles
	writeChan chanIn (_bot, snake, food, obstacles) -- computes BFS on threadPool
	threadDelay gameP 								-- while gameLoop sleeps
	botDir <- readChan chanOut
	let bot = (fst _bot, botDir)
	snakeMoveAction snake bot food obstacles >>= (\(movedSnake, food2, status) -> do
			putMVar mSnake movedSnake
			if status == VALID then
				if(length (fst movedSnake) < maxLenP) then do 
					snakeMoveAction bot movedSnake food2 obstacles >>= (\(movedBot, food3, statusBot) ->
						if statusBot == VALID then
							if length (fst movedBot) < maxLenB then
								gameLoop mSnake movedBot food3 (level, gameP, maxLenP, maxLenB, obstacles) (chanIn, chanOut)
							else do
								printBoard movedSnake movedBot food3 obstacles
								return DEFEAT_FOOD
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
	threadDelay $ 10^5

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
	-- creating threadPool for BFS with only one thread
	chans <- threadPoolIO 1 computeDirection
	putStrLn "\n\n\n\nPara se movimentar: [A,S,W,D] ou [Setinhas]"
	putStrLn "LEVEL 1"
	putStrLn "Digite algo para começar."
	getChar
	forkIO $ keyListener mSnake
	gameLoop mSnake bot food level1 chans >>= (\result -> 
		if result == WIN 
			then
				do
					putStrLn "LEVEL 2"
					putStrLn "Se prepare!"
					threadDelay $ 3*(10^6)
					takeMVar mSnake >>= (\_ -> putMVar mSnake newSnake)
					food2 <- newFood newSnake bot obstaclesLevel2
					gameLoop mSnake bot food2 level2 chans >>= (\result ->  -- second level call
						if result == WIN 
							then
								do
									putStrLn "LEVEL 3"
									putStrLn "Se prepare!\n\n"
									threadDelay $ 3*(10^6)
									takeMVar mSnake >>= (\_ -> putMVar mSnake newSnake)
									food3 <- newFood newSnake bot obstaclesLevel3
									gameLoop mSnake bot food3 level3 chans >>= (\result -> printGameResult result)
							else 
								printGameResult result)
			else 
				printGameResult result
		)

