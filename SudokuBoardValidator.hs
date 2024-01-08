-- https://www.codewars.com/kata/63d1bac72de941033dbf87ae
-- Sudoku board validator

module SudokuBoardValidator (validateSudoku) where

import Data.List(sort,transpose)
import Data.List.Split(chunksOf)

type Sudoku = [[Int]]

validateSudoku :: Sudoku -> Bool
validateSudoku board = and [ checkHorizontal board, checkVertical board, check3x3 board ]

check :: [Int] -> Bool
check line = sort line == [1..9]

checkHorizontal :: Sudoku -> Bool
checkHorizontal board = and [ check l | l <- board ]

checkVertical :: Sudoku -> Bool
checkVertical board = and [ check l | l <- transpose board ]

check3x3 :: Sudoku -> Bool
check3x3 board = let
    b3 = splitAt3 board
    in and 
        [ and [ 
            check $ concat [x1,y1,z1], 
            check $ concat [x2,y2,z2], 
            check $ concat [x3,y3,z3] ]
            | [l1,l2,l3] <- b3, 
            let [x1,x2,x3] = splitAt3 l1, 
            let [y1,y2,y3] = splitAt3 l2,
            let [z1,z2,z3] = splitAt3 l3 ]

splitAt3 xs = let 
    (xs1,xs') = splitAt 3 xs
    (xs2,xs3) = splitAt 3 xs' 
    in [xs1,xs2,xs3]

{-
数独のバリデータ。
数独がちゃんと解けているか、
縦、横、3x3ごとに1~9がそれぞれあるかどうかチェックする。
-}