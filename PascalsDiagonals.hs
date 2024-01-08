-- https://www.codewars.com/kata/576b072359b1161a7b000a17
-- Pascal's Diagonals

module PascalsDiagonals (generateDiagonal) where

generateDiagonal :: Int -> Int -> [Int]
generateDiagonal d n = f d (replicate n 1)
    where
        f 0 xs = xs
        f d xs = f (d-1) (pascal xs)


tri 0 n = 1
tri d n = sum [ tri (d-1) i | i <- [1..n] ]

pascal xs = [ sum $ take i xs | i <- [1..length xs] ]
{-
パスカルの三角形を斜めに見て行った時の数列を取り出す。
定義通りにやると計算量が多い。

こうしてみたらどうか。
最初に１がn個のリストを用意する。
そのリストを元に、最初の１個、最初の２個の和、最初の３個の和、・・・を求める。
これを指定の回数繰り返す。
-}