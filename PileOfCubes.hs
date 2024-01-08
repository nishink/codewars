-- https://www.codewars.com/kata/5592e3bd57b64d00f3000047
-- Build a pile of Cubes

module Codewars.Kata.PileOfCubes where

findNb :: Integer -> Integer
findNb m = f 1 0
    where
        f n acc = let
            m' = acc + n ^ 3
            in if m == m' then n else if m < m' then -1 else f (n+1) m'


{-
n == 1 -> 1
n == 2 -> 1^3 + 2^3 = 9
n == 3 -> 9 + 3^3 = 36
n == 4 -> 36 + 4^3 = 100
  : 
n == n -> (sum [1..n]) ^ 2

-}