-- https://www.codewars.com/kata/57dc535fd8f9291a0300030d
-- Sliding tile puzzle - Worker

module Haskell.Codewars.SlidingPuzzleWorker where

import Data.List
import Data.Maybe

slideTiles :: [[Int]] -> [Int] -> [[Int]]
slideTiles board [] = board
slideTiles board (s:ss) = slideTiles [ slide tiles s | tiles <- board ] ss

set :: [Int] -> Int -> Int -> [Int]
set xs idx val = take idx xs ++ [val] ++ drop (idx+1) xs

slide :: [Int] -> Int -> [Int]
slide tiles x = let
    idx = elemIndex x tiles
    idx0 = elemIndex 0 tiles
    tmp = if idx == Nothing then tiles else set tiles (fromJust idx) 0
    in if idx0 == Nothing then tmp else set tmp (fromJust idx0) x

{-
slideTiles :: [[Int]] -> [Int] -> [[Int]]
スライドパズルの初期状態 -> スライドするタイル -> スライドした結果

      let puzzle3x3    = [[7,3,1],[4,6,0],[8,2,5]]
      let instructions = [5,2,8,4]
      (slideTiles puzzle3x3 instructions) `shouldBe`
        [[7,3,1],[0,6,5],[4,8,2]] 

[7,3,1],
[4,6,0],
[8,2,5]

これを[5,2,8,4]と動かすと

[7,3,1],
[0,6,5],
[4,8,2]

考え方としては、常に0の隣にあるものを動かすことになるので、
0と位置を交換していけばよい。

パズルは3x3とは限らないため、
一列に並べた後元に戻すというのも難しそうなので、
二次元配列のまま処理する。

-}
