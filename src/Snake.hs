module Snake where

import Definitions

type Snake = ([Position], Direction)

{-	
	movePosition:
	Dada uma posição, retorna uma posição deslocada na direção especificada
	Usado em Move Snake para mover a cabeça da cobra 
-}
movePosition :: Position -> Direction -> Position
movePosition pos LEFT = (fst pos, (snd pos)-1)
movePosition pos UP = ((fst pos)-1, snd pos)
movePosition pos RIGHT = (fst pos, (snd pos)+1)
movePosition pos DOWN = ((fst pos)+1, snd pos)

{-	
	moveSnake:
	Dada uma cobra, retorna a cobra deslocada na direção especificada
	Usa movePosition para mover a cabeça da cobra e concatena
		com o resto da cobra para poder realizar o movimento inteiro e
		descartar a ultima posição da cobra (já que a cabeça foi deslocada)
-}
moveSnake :: Snake -> Snake
moveSnake (snake, dir) = ((nextPosition (snake, dir)):(init snake), dir)

{-	
	snakeStatus:
	Dada uma cobra, retorna True se a cobra está situada validamente no tabuleiro
	Deve ser usado a cada movimento para detectar um movimento errado assim
		que a cabeça da cobra sair do tabuleiro
-}
snakeStatus :: Snake -> Int -> SnakePositionStatus
snakeStatus ((snake), _) boardSize = do
	if (snakeHead `elem` tail snake) then
		HIT_ITSELF
	else
		if 	((fst snakeHead < 1) || (fst snakeHead > boardSize) 
			|| (snd snakeHead < 1) || (snd snakeHead > boardSize)) then
			HIT_WALL
		else
			VALID
				where snakeHead = head snake

nextPosition :: Snake -> Position
nextPosition (snake, dir) = movePosition (head snake) dir

{-
	computeDirection:
	Dada a cobra da IA, a cobra do player, a comida (Position)
	computar a direção para qual a IA deve ir
	Algoritmo: BFS (busca em largura)	
-}

checkPos :: Snake -> Snake -> Position -> Bool
checkPos (s1, _) (s2, _) pos = not (pos `elem` s1 || pos `elem` s2)

nextPoss :: Position -> [Position]
nextPoss (x,y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

_computeDirection :: Snake -> Snake -> Position -> [(Position, Int)] -> [(Position, Int)] -> [Position] -> ([(Position, Int)], Int)
_computeDirection s1 s2 f (x:xs) proc marks
	| fst x == f = (proc, snd x)
	| otherwise = _computeDirection s1 s2 f newExplorer newTracker newMarker
		where 
			newPoss = filter (\pos -> (not $ pos `elem` marks) && (checkPos s1 s2 pos) && (validPosition pos)) (nextPoss $ fst x)
			len = length proc
			newExplorer = xs ++ [(fst v, ((snd v) + len)) | v <- zip newPoss [0..]]
			newTracker = proc ++ [(pos, snd x) | pos <- newPoss]
			newMarker = marks ++ newPoss

getFirstMove :: ([(Position, Int)], Int) -> Position
getFirstMove (trace, curIdx)
	| prevIdx == -1 = fst $ cur
	| otherwise = getFirstMove (trace, snd cur)
	where
		cur = trace !! curIdx
		prevIdx = snd $ trace !! (snd cur)

getFirstMoveDirection :: Position -> Position -> Direction 
getFirstMoveDirection (r0, c0) (r1, c1)
	| (r0-r1) == 1 = UP
	| (r0-r1) == -1 = DOWN
	| (c0-c1) == 1 = LEFT
	| (c0-c1) == -1 = RIGHT
	| otherwise = error "BFS ERROR - NO DIRECTION POSSIBLE" 

-- computeDirection :: Snake -> Snake -> Position -> Direction
computeDirection :: Snake -> Snake -> Position -> Direction
computeDirection s1 s2 f = 
	getFirstMoveDirection botSnakeHead (getFirstMove (_computeDirection s1 s2 f [((x, y), 0)] [((x, y), -1)] [(x,y)]) )
		where
			(x, y) = head $ fst s1
			botSnakeHead = head $ fst s1