module Level where

import Definitions
import Obstacle

--nivel, gamePace e lista de obstaculos
type Level = (Int, Int, [Obstacle])

Level1 :: Level
Level1 = (1, gamePace!!0, obstaclesLevel1)

Level2 :: Level
Level2 = (2, gamePace!!1, obstaclesLevel2)

Level3 :: Level
Level3 = (3, gamePace!!2, obstaclesLevel3)