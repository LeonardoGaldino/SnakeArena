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