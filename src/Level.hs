module Level where

import Definitions
import Obstacle

--nivel, gamePace e lista de obstaculos
type Level = (Int, Int, [Obstacle])

level1 :: Level
level1 = (1, gamePace!!0, obstaclesLevel1)

level2 :: Level
level2 = (2, gamePace!!1, obstaclesLevel2)

level3 :: Level
level3 = (3, gamePace!!2, obstaclesLevel3)