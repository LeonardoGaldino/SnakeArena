module Level where

import Definitions
import Obstacle

--nivel, gamePace e lista de obstaculos
type Level = (Int, Int, [Obstacle])


Lvl1 :: Level
Lvl1 = (1, gamePace!!0, [])

Lvl2 :: Level
Lvl2 = (2, gamePace!!1, obstacles2)

Lvl3 :: Level
Lvl3 = (3, gamePace!!2, obstacles3)