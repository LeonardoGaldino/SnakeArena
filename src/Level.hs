module Level where

import Definitions
import Obstacle

-- nivel, 
-- gamePace 
-- tamanho máximo para passar de level 
-- tamanho máximo para o bot vencer o player
-- lista de obstaculos
type Level = (Int, Int, Int, Int, [Obstacle])

level1 :: Level
level1 = (1, (10^4) * 21, 8, 13, obstaclesLevel1)

level2 :: Level
level2 = (2, (10^4) * 15, 13, 23, obstaclesLevel2)

level3 :: Level
level3 = (3, 10^5, (boardSize*boardSize)+1, (boardSize*boardSize)+1, obstaclesLevel3)

nextLevel :: Level -> Level
nextLevel (1, _, _, _, _) = level2
nextLevel (2, _, _, _, _) = level3
nextLevel _ = level1

nextLevelByLevelNum :: Int -> Level
nextLevelByLevelNum 1 = level2
nextLevelByLevelNum 2 = level3
nextLevelByLevelNum _ = level1
