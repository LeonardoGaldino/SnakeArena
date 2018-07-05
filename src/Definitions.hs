module Definitions where

import Control.Concurrent.Chan -- channels for threadPool

import Graphics.Gloss.Data.Color

type Position = (Int, Int)

data Direction = LEFT | UP | RIGHT | DOWN
	deriving (Eq, Show)

data GameResult = WIN | DEFEAT_WALL | DEFEAT_ITSELF | DEFEAT_COLLISION | DEFEAT_FOOD deriving (Eq)
data SnakeStatus = VALID | HIT_WALL | HIT_ITSELF | COLLISION_WIN | COLLISION_DEFEAT
	deriving (Eq, Ord, Show)

mapSnakeStatusGameResult :: SnakeStatus -> Bool -> GameResult
mapSnakeStatusGameResult HIT_WALL True = DEFEAT_WALL
mapSnakeStatusGameResult HIT_WALL False = WIN
mapSnakeStatusGameResult HIT_ITSELF True = DEFEAT_ITSELF
mapSnakeStatusGameResult HIT_ITSELF False = WIN
mapSnakeStatusGameResult COLLISION_WIN True = WIN
mapSnakeStatusGameResult COLLISION_WIN False = DEFEAT_COLLISION
mapSnakeStatusGameResult COLLISION_DEFEAT True = DEFEAT_COLLISION
mapSnakeStatusGameResult COLLISION_DEFEAT False = WIN

boardSize :: Int
boardSize = 24

unitSize :: Float
unitSize = 25.0

gameName :: String
gameName = "Snake Arena"

iterationsPerSec :: Int
iterationsPerSec = 7

windowSize :: (Int, Int)
windowSize = ((truncate unitSize)*boardSize + 30, (truncate unitSize)*boardSize + 30)

windowPos :: (Int, Int)
windowPos = (100, 100)

windowBackgroundColor :: Color
windowBackgroundColor = makeColor 0.5 0.5 0.5 1

snakeHeadRadius :: Float
snakeHeadRadius = 10.0

snakeTailRadius :: Float
snakeTailRadius = 8.0

foodSize :: Float
foodSize = 15.0

foodColor :: Color
foodColor = makeColor 0.3 1 0 1 

obstacleSize :: Float
obstacleSize = 30.0

obstacleColor :: Color
obstacleColor = makeColor 1 0 0 1 

playerHeadColor :: Color
playerHeadColor = makeColor 0.8 0.3 0.1 1

playerTailColor :: Color
playerTailColor = makeColor 0.3 0.2 0.1 1

botHeadColor :: Color
botHeadColor = makeColor 0.4 0.7 0.5 1

botTailColor :: Color
botTailColor = makeColor 0.8 0.1 0.3 1


positionToPixel :: Position -> (Float, Float)
positionToPixel (x,y) = ((fromIntegral y*unitSize) - halfBoard*unitSize - (unitSize/fromIntegral boardSize) - 12, 
	halfBoard*unitSize - (fromIntegral x*unitSize) + (unitSize/fromIntegral boardSize) + 12)
		where
			halfBoard = (fromIntegral boardSize)/2

validPosition :: Position -> Bool
validPosition (x,y) = (x >= 1 && x <= boardSize && y >= 1 && y <= boardSize)

counterDirection :: Direction -> Direction
counterDirection LEFT = RIGHT
counterDirection UP = DOWN
counterDirection RIGHT = LEFT
counterDirection DOWN = UP

updateDirection :: Direction -> Direction -> Direction
updateDirection curDir newDir = ( (curDir == (counterDirection newDir)) ? (curDir, newDir))

mapCharDirection :: Char -> Direction
mapCharDirection 'a' = LEFT
mapCharDirection 'D' = LEFT -- LEFT ARROW
mapCharDirection 'w' = UP -- UP ARROW 
mapCharDirection 'A' = UP
mapCharDirection 'd' = RIGHT
mapCharDirection 'C' = RIGHT -- RIGHT ARROW
mapCharDirection 's' = DOWN
mapCharDirection 'B' = DOWN -- DOWN ARROW

(?) :: Bool -> (a, a) -> a
True  ? (x, _) = x
False ? (_, y) = y

printGameResult :: GameResult -> IO ()
printGameResult WIN = putStrLn "Parabéns, você venceu!"
printGameResult DEFEAT_WALL = putStrLn "Parabéns, você perdeu ao colidir com a parede!"
printGameResult DEFEAT_ITSELF = putStrLn "Parabéns, você perdeu ao colidir consigo mesmo!"
printGameResult DEFEAT_COLLISION = putStrLn "Parabéns, você perdeu ao colidir com o adversário maior que você!"
printGameResult DEFEAT_FOOD = putStrLn "Parabéns, você perdeu pois o BOT comeu muitas comidas"