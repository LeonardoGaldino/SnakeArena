module Definitions where

type Position = (Int, Int)

data Direction = LEFT | UP | RIGHT | DOWN
	deriving (Eq, Show)

data GameResult = WIN | DEFEAT_WALL | DEFEAT_ITSELF | DEFEAT_COLLISION deriving (Eq)
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
boardSize = 20

gameName :: String
gameName = "Snake Arena"

gamePace :: Int
gamePace = 10^5

validPosition :: Position -> Bool
validPosition (x,y) = (x >= 1 && x <= boardSize && y >= 1 && y <= boardSize)

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
printGameResult DEFEAT_COLLISION = putStrLn "Parabéns, você perdeu ao colidir com o adversário maior que você!"