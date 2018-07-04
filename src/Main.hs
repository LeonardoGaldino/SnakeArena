module Main where

-- Dependencies imports
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Display
import Graphics.Gloss.Interface.IO.Game

-- Standard imports
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Char
import System.Exit

-- Custom files imports
import Snake
import Board
import Food
import Definitions
import Obstacle
import Level

initPlayer :: Snake
initPlayer = ([(1,3), (1,2), (1,1)], RIGHT)

initBot :: Snake
initBot = ([(boardSize,3), (boardSize,2), (boardSize,1)], RIGHT)

instanciateWorld :: Level -> IO World
instanciateWorld (levelNum, pace, maxWin, maxDefeat, obss) = do
	f <- (newFood initPlayer initBot obss)
	mResult <- newMVar RIGHT
	forkIO $ computeDirection initBot initPlayer f obss mResult
	return (World initPlayer initBot f (levelNum, pace, maxWin, maxDefeat, obss) mResult)

drawWorld :: World -> IO Picture
drawWorld (World player bot food (_,_,_,_,obstacles) _) = return (Pictures [obssPictures, foodPicture, playerPicture, botPicture])
	where
		obssPictures = drawObstacles obstacles
		foodPicture = drawFood food
		playerPicture = drawSnake player playerHeadColor playerTailColor
		botPicture = drawSnake bot botHeadColor botTailColor

handler :: Event -> World -> IO World
-- Handling DOWN command
handler (EventKey (SpecialKey KeyLeft) Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir LEFT) b c d e) 
handler (EventKey (Char 'a') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir LEFT) b c d e) 
-- Handling UP command
handler (EventKey (SpecialKey KeyUp) Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir UP) b c d e)
handler (EventKey (Char 'w') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir UP) b c d e)
-- Handling RIGHT command
handler (EventKey (SpecialKey KeyRight) Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir RIGHT) b c d e)
handler (EventKey (Char 'd') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir RIGHT) b c d e)
-- Handling DOWN command
handler (EventKey (SpecialKey KeyDown) Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir DOWN) b c d e)
handler (EventKey (Char 's') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir DOWN) b c d e)
-- Any other COMMAND must be ignored ~HEXA VEM~
handler _ w = return w

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

--                          Snake                   Snake           Food              Level                 
data World = World ([Position], Direction) ([Position], Direction) Position (Int, Int, Int, Int, [Obstacle]) (MVar Direction)

gameLoop :: Float -> World -> IO World
gameLoop _ (World player _bot food (level, gameP, maxLenP, maxLenB, obstacles) mResult) = do
	botDir <- takeMVar mResult
	putMVar mResult botDir
	let bot = (fst _bot, botDir)
	snakeMoveAction player bot food obstacles >>= (\(movedPlayer, food2, status) ->
		if status == VALID then
			if(length (fst movedPlayer) < maxLenP) then 
				snakeMoveAction bot movedPlayer food2 obstacles >>= (\(movedBot, food3, statusBot) ->
					if statusBot == VALID then
						if length (fst movedBot) < maxLenB then do
							forkIO $ computeDirection movedBot movedPlayer food3 obstacles mResult
							return (World movedPlayer movedBot food3 (level, gameP, maxLenP, maxLenB, obstacles) mResult)
						else
							instanciateWorld level1
					else
						if status == COLLISION_WIN then
							instanciateWorld level1
						else
							instanciateWorld $ nextLevelByLevelNum level
					)
			else -- player wins
				instanciateWorld $ nextLevelByLevelNum level
		else
			if status == COLLISION_WIN then
				instanciateWorld $ nextLevelByLevelNum level
			else
				instanciateWorld level1
		)

main :: IO ()
main = do
	initialWorld <- instanciateWorld level1
	let displaySettings = InWindow gameName windowSize windowPos
	playIO displaySettings windowBackgroundColor 10 initialWorld drawWorld handler gameLoop