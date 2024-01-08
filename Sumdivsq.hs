-- https://www.codewars.com/kata/55aa075506463dac6600010d
-- Integers: Recreation One

module Codewars.G964.Sumdivsq where

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n = [ (i, j) | i <- [m..n], let j = sum $ map (^2) (divisors i), isSquared j ]

divisors n = [ x | x <- [1..n], n `rem` x == 0 ]

isSquared n = let
    m = floor $ sqrt $ fromIntegral n
    in m ^ 2 == n