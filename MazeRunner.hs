-- https://www.codewars.com/kata/58663693b359c4a6560001d6
-- Maze Runner

module Haskell.SylarDoom.MazeRunner where

import Data.List

mazeRunner :: [[Int]] -> [Char] -> [Char]
mazeRunner maze directions = let
    sp = getPos 2 maze
    fp = getPos 3 maze
    in run directions sp fp
    where
        run [] _ _ = "Lost"
        run (d:ds) p fp = let
            np = walk d p
            obj = getObj maze np
            in if np == fp then "Finish"
                else if obj == -1 || obj == 1 then "Dead"
                else run ds np fp

getPos :: Int -> [[Int]] -> (Int,Int)
getPos p maze = getP maze 0
    where
        getP (x:xs) n = if elem p x then (head (elemIndices p x), n) else getP xs (n+1)

getObj :: [[Int]] -> (Int,Int) -> Int
getObj maze@(m:ms) (x,y) = 
    if x < 0 || y < 0 || length m <= x || length maze <= y then -1
    else maze !! y !! x

walk :: Char -> (Int,Int) -> (Int,Int)
walk 'N' (x,y) = (x,y-1)
walk 'E' (x,y) = (x+1,y)
walk 'W' (x,y) = (x-1,y)
walk 'S' (x,y) = (x,y+1)


maze :: [[Int]]
maze = [[1,1,1,1,1,1,1],
        [1,0,0,0,0,0,3],
        [1,0,1,0,1,0,1],
        [0,0,1,0,0,0,1],
        [1,0,1,0,1,0,1],
        [1,0,0,0,0,0,1],
        [1,2,1,0,1,0,1]]
