-- https://www.codewars.com/kata/583d171f28a0c04b7c00009c
-- The maximum sum value of ranges -- Challenge version

module Kata.MaxSumValRanges (maxSum) where

import Data.Array

maxSum :: [Int] -> [(Int,Int)] -> Int
maxSum list ranges = let
    totals = listArray (0, length list - 1) $ scanl1 (+) list
    in fnc totals ranges minBound
    where
        fnc _ [] m = m
        fnc ts ((f,l):rs) m = let
            v = (ts ! l) - (if f == 0 then 0 else ts ! (f - 1))
            in fnc ts rs (max m v)

a :: [Int]
a = [1, -2, 3, 4, -5, -4, 3, 2, 1]
ranges :: [(Int,Int)]
ranges = [(1, 3), (0, 4), (6, 8)]
{-
整数のリストが与えられたとき、
指定した範囲の合計を計算し、その最大値を求める。

例：
A = [1, -2, 3, 4, -5, -4, 3, 2, 1]
ranges = [(1, 3), (0, 4), (6, 8)]

result = 6

For ranges[0] = (1, 3) the sum is A[1] + A[2] + A[3] = 5
For ranges[1] = (0, 4) the sum is A[0] + A[1] + A[2] + A[3] + A[4] = 1
For ranges[2] = (6, 8) the sum is A[6] + A[7] + A[8] = 6
最大の合計は6

例の通りに計算すると、範囲が重複しているので、
同じ範囲の計算を何度もすることになり、効率が悪い。

最初に、累計を計算するのはどうか。
例：
元のリスト
A = [1, -2, 3, 4, -5, -4, 3, 2, 1]
累計のリスト
B = [1, -1, 2, 6, 1, -3, 0, 2, 3]

rangeの計算は、
(first, last) -> range = B[last] - B[first-1]
で求められる。

最初、listのまま計算してみたが、タイムアウトになってしまった。
何がボトルネックなのかを探るため、まずはmaximumを使わないでmaxで比較しながら最大を求めるようにしたが、
やはりタイムアウトしてしまった。
listをarrayにしてみたところ、解消した。

Intの最大値、最小値はmaxBound, minBoundを使えば良いというのを学んだ。
-}
