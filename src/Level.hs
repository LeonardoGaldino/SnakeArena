module Level where

import Definitions
import Obstacle

--nivel, gamePace, tamanho máximo para passar de level, lista de obstaculos
type Level = (Int, Int, Int, [Obstacle])

level1 :: Level
level1 = (1, gamePace!!0, 8, obstaclesLevel1)

level2 :: Level
level2 = (2, gamePace!!1, 13, obstaclesLevel2)

level3 :: Level
level3 = (3, gamePace!!2, (boardSize*boardSize)+1, obstaclesLevel3)