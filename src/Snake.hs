module Snake where

import Control.Concurrent.MVar

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
snakeStatus :: Snake -> Snake -> SnakeStatus
snakeStatus (snake, _) (enemy, _) = do
	if (snakeHead `elem` tail snake) then
		HIT_ITSELF
	else
		if 	((fst snakeHead < 1) || (fst snakeHead > boardSize) 
			|| (snd snakeHead < 1) || (snd snakeHead > boardSize)) then
			HIT_WALL
		else
			if snakeHead `elem` enemy then
				if length snake >= length enemy then
					COLLISION_WIN
				else
					COLLISION_DEFEAT
			else
				VALID
					where snakeHead = head snake

nextPosition :: Snake -> Position
nextPosition (snake, dir) = movePosition (head snake) dir

{-
	checkPos:
	Checa se uma posição colide com uma das duas cobras no mapa
-}
checkPos :: Snake -> Snake -> Position -> Bool
checkPos (s1, _) (s2, _) pos = not (pos `elem` s1 || pos `elem` s2)


{-
	nextPoss:
	Retorna todas as possíveis próximas posições de uma posição.
	(pra cima, pra baixo, pra esquerda, pra direita)
-}
nextPoss :: Position -> [Position]
nextPoss (r,c) = [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

{-
	_computeDirection:
	Função auxiliar da BFS: procura a comida		
-}
_computeDirection :: Snake -> Snake -> Position -> [(Position, Int)] -> [(Position, Int)] -> [Position] -> ([(Position, Int)], Int)
_computeDirection _ _ _ [] _ _ = ([], -2)
_computeDirection s1 s2 f (x:xs) proc marks
	| fst x == f = (proc, snd x)
	| otherwise = _computeDirection s1 s2 f newExplorer newTracker newMarker
		where 
			newPoss = filter (\pos -> (not $ pos `elem` marks) && (checkPos s1 s2 pos) && (validPosition pos)) (nextPoss $ fst x)
			len = length proc
			newExplorer = xs ++ [(fst v, ((snd v) + len)) | v <- zip newPoss [0..]]
			newTracker = proc ++ [(pos, snd x) | pos <- newPoss]
			newMarker = marks ++ newPoss

{-
	getFirstMove:
	Dada uma sequencia de posições e o indice da posição anterior,
	computa qual foi a posição que sucedeu a posição inicial no caminho que levou à comida
-}
getFirstMove :: ([(Position, Int)], Int) -> Position
getFirstMove (trace, curIdx)
	| prevIdx == -1 = fst $ cur
	| otherwise = getFirstMove (trace, snd cur)
	where
		cur = trace !! curIdx
		prevIdx = snd $ trace !! (snd cur)

{-
	getFirstMoveDirection:
	Computa qual a direção que tomada para levar da posição1
	para a posição2.
	*Posição1 e Posição2 devem ser vizinhas (não considerando diagonais)
-}
getFirstMoveDirection :: Position -> Position -> Direction 
getFirstMoveDirection (r0, c0) (r1, c1)
	| (r0-r1) == 1 = UP
	| (r0-r1) == -1 = DOWN
	| (c0-c1) == 1 = LEFT
	| (c0-c1) == -1 = RIGHT
	| otherwise = error "BFS ERROR - NO DIRECTION POSSIBLE" 

{-
	computeDirection:
	Dada a cobra da IA, a cobra do player, a comida (Position)
	computar a direção para qual a IA deve ir
	Algoritmo: BFS (busca em largura)
	Feita pra rodar em outra thread separada
-}
computeDirection :: Snake -> Snake -> Position -> MVar Direction -> IO ()
computeDirection s1 s2 f mResult = do
	takeMVar mResult
	if resultIdx == -2 then -- -2 sinalizes unreachable food
		if not (toLeft `elem` fst s1) then
			putMVar mResult LEFT
		else
			if not (toUp `elem` fst s1) then
				putMVar mResult UP
			else
				if not (toRight `elem` fst s1) then
					putMVar mResult RIGHT
				else
					putMVar mResult DOWN
	else
		putMVar mResult $ getFirstMoveDirection (r, c) (getFirstMove bfsResult)
			where
				(r, c) = head $ fst s1
				toLeft = movePosition (r, c) LEFT
				toUp = movePosition (r, c) UP
				toRight = movePosition (r, c) RIGHT
				bfsResult = _computeDirection s1 s2 f [((r, c), 0)] [((r, c), -1)] [(r,c)]
				resultIdx = snd bfsResult