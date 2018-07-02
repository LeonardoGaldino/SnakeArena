module Definitions where

type Position = (Int, Int)

data Direction = LEFT | UP | RIGHT | DOWN
	deriving (Eq)

data GameResult = WIN | DEFEAT_WALL | DEFEAT_ITSELF deriving (Eq)
data SnakePositionStatus = VALID | HIT_WALL | HIT_ITSELF
	deriving (Eq, Ord, Show)

gameName :: String
gameName = "Snake Arena"

gamePace :: Int
gamePace = 10^5

counterDirection :: Direction -> Direction
counterDirection LEFT = RIGHT
counterDirection UP = DOWN
counterDirection RIGHT = LEFT
counterDirection DOWN = UP

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