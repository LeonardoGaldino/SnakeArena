module Definitions where

type Position = (Int, Int)

data Direction = LEFT | UP | RIGHT | DOWN

counterDirection :: Direction -> Direction
counterDirection LEFT = RIGHT
counterDirection RIGHT = LEFT
counterDirection DOWN = UP
counterDirection UP = DOWN

