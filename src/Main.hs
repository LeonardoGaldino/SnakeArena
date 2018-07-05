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

{-
	Initial Player state in any level	
-}
initPlayer :: Snake
initPlayer = ([(1,3), (1,2), (1,1)], RIGHT)

{-
	Initial Bot state in any level	
-}
initBot :: Snake
initBot = ([(boardSize,3), (boardSize,2), (boardSize,1)], RIGHT)

{-
	Given a level,
	output the world in its initial state in that level	
-}
instanciateWorld :: Level -> IO World
instanciateWorld (levelNum, pace, maxWin, maxDefeat, obss) = do
	f <- (newFood initPlayer initBot obss)
	mResult <- newMVar RIGHT
	forkIO $ computeDirection initBot initPlayer f obss mResult
	return (World initPlayer initBot f (levelNum, pace, maxWin, maxDefeat, obss) mResult)

instanciateWorldWIN :: IO World
instanciateWorldWIN = do
	temp <- newEmptyMVar
	return (World ([], RIGHT) ([], RIGHT) (boardSize+5, boardSize+5) (4, 1, 1, 1, obstaclesLevelWIN) temp)
	
{-
	Function designed to draw output a Picture out of a 'world' (a game state)
	Used by gross lib	
-}
drawWorld :: World -> IO Picture
drawWorld (World player bot food (_,_,_,_,obstacles) _) = return (Pictures [obssPictures, foodPicture, playerPicture, botPicture])
	where
		obssPictures = drawObstacles obstacles
		foodPicture = drawFood food
		playerPicture = drawSnake player playerHeadColor playerTailColor
		botPicture = drawSnake bot botHeadColor botTailColor

{-
	Handlers for player interaction
-}
handler :: Event -> World -> IO World
-- Handling DOWN command
handler (EventKey (SpecialKey KeyLeft) Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir LEFT) b c d e) 
handler (EventKey (Char 'a') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir LEFT) b c d e) 
handler (EventKey (Char 'A') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir LEFT) b c d e) 
-- Handling UP command
handler (EventKey (SpecialKey KeyUp) Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir UP) b c d e)
handler (EventKey (Char 'w') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir UP) b c d e)
handler (EventKey (Char 'W') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir UP) b c d e)
-- Handling RIGHT command
handler (EventKey (SpecialKey KeyRight) Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir RIGHT) b c d e)
handler (EventKey (Char 'd') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir RIGHT) b c d e)
handler (EventKey (Char 'D') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir RIGHT) b c d e)
-- Handling DOWN command
handler (EventKey (SpecialKey KeyDown) Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir DOWN) b c d e)
handler (EventKey (Char 's') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir DOWN) b c d e)
handler (EventKey (Char 'S') Down _ (_, _)) (World (a, dir) b c d e) = return (World (a, updateDirection dir DOWN) b c d e)
-- Handling Exit key
handler (EventKey (Char 'q') Down _ (_, _)) _ = exitSuccess
handler (EventKey (Char 'Q') Down _ (_, _)) _ = exitSuccess
handler (EventKey (SpecialKey KeyEsc) Down _ (_, _)) _ = exitSuccess
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


-- Type that defines *a game state*
data World = World Snake Snake Food Level (MVar Direction)

gameLoop :: Float -> World -> IO World
gameLoop _ (World a b c (4, d, e, f, g) h) = instanciateWorldWIN
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
	playIO displaySettings windowBackgroundColor iterationsPerSec initialWorld drawWorld handler gameLoop