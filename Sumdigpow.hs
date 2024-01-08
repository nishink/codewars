-- https://www.codewars.com/kata/5626b561280a42ecc50000d1
-- Take a Number And Sum Its Digits Raised To The Consecutive Powers And ....¡Eureka!!

module Codewars.G964.Sumdigpow where

import Data.Char

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = [ n | n <- [a..b], sdp n == n ]

sdp n = sum [ (digitToInt d) ^ p | (d, p) <- zip (show n) [1..] ]
