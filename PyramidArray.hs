-- https://www.codewars.com/kata/515f51d438015969f7000013
-- Pyramid Array

module Kata where

pyramid :: Int -> [[Int]]
pyramid n = [replicate i 1 | i <- [1..n]]

{-
整数n >= 0が与えられた関数を記述すると、長さのn個の昇順の部分配列を返し、すべて1で満たされます。

0 => [ ]
1 => [ [1] ]
2 => [ [1], [1, 1] ]
3 => [ [1], [1, 1], [1, 1, 1] ]
-}