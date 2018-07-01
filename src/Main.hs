module Main where

-- Dependencies imports
import Graphics.UI.Fungen


-- Custom files imports
import Snake
import Board
import Definitions

main :: IO ()
main = do
	let map = initialBoard
	putStrLn $ show map
	putStrLn "End Main"
