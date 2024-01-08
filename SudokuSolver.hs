-- https://www.codewars.com/kata/5296bc77afba8baa690002d7
-- Sudoku Solver

-- 数独を解くプログラム
module Sudoku where

import Data.List
import Data.Function

sudoku :: [[Int]] -> [[Int]]
sudoku puzzle = case solve $ toBoard puzzle of
        answer : _ -> format answer
        []         -> []


type Cell = (Int, Int) -- マス
type Board = [(Cell, Int)] -- 盤面

-- solve : 数独を解く関数
solve :: Board -> [Board]
solve board | length board == 81 = [board]
solve board = [ (cell, n) : board
            | let remains = cells \\ map fst board
            , let cell = maximumBy (compare `on` length . used board) remains
            , n <- [1..9] \\ used board cell
            ] >>= solve

-- cells : 81マス全体を表す
cells :: [Cell]
cells = [ (x, y) | x <- [0..8], y <- [0..8] ]

-- マスの所属する区画
-- | 0 | 1 | 2 |
-- | 3 | 4 | 5 |
-- | 6 | 7 | 8 |
area :: Cell -> Int
area (x, y) = y `div` 3 * 3 + x `div` 3

-- あるマスの周囲で使われている数を列挙
used :: Board -> Cell -> [Int]
used board cell = nub [ n
                | (cell', n) <- board
                , any (\f -> f cell == f cell') [ snd, fst, area ]
                ]

-- 解を整形
format :: Board -> [[Int]]
format = map (map snd) . transpose . groupBy ((==) `on` (fst . fst)) . sort

-- 問題を整形
toBoard :: [[Int]] -> Board
toBoard puzzle = [ ((x, y), n) | x <- [0..8], y <- [0..8], let n = puzzle !! y !! x, n /= 0 ]

main :: IO ()
main = case solve problem of
        answer : _ -> mapM_ print $ format answer
        []         -> putStrLn "invalid problem"


-- 例題
problem :: Board
problem = [ ((3, 0), 8) -- 座標(3, 0)に8が書かれている、という意味
        ,   ((5, 0), 1)
        ,   ((6, 1), 4)
        ,   ((7, 1), 3)
        ,   ((0, 2), 5)
        ,   ((4, 3), 7)
        ,   ((6, 3), 8)
        ,   ((6, 4), 1)
        ,   ((1, 5), 2)
        ,   ((4, 5), 3)
        ,   ((0, 6), 6)
        ,   ((7, 6), 7)
        ,   ((8, 6), 5)
        ,   ((2, 7), 3)
        ,   ((3, 7), 4)
        ,   ((3, 8), 2)
        ,   ((6, 8), 6)
        ]
{-
例題を盤面にすると下記のようになる。
0|_ _ _ |_8_ _1|_ _ _ |
1|_ _ _ |_ _ _ |_4_3_ |
2|_5_ _ |_ _ _ |_ _ _ |_
3|_ _ _ |_ _7_ |_8_ _ |
4|_ _ _ |_ _ _ |_1_ _ |
5|_ _2_ |_ _3_ |_ _ _ |_
6|_6_ _ |_ _ _ |_ _7_5|
7|_ _ _3|_4_ _ |_ _ _ |
8|_ _ _ |_2_ _ |_6_ _ |
   0 1 2  3 4 5  6 7 8
-}

puzzle :: [[Int]]
puzzle = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]]
