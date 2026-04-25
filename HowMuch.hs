-- https://www.codewars.com/kata/55b4d87a3766d9873a0000d4
-- How Much?

module Codewars.G964.Carboat where

howmuch :: Int -> Int -> [[String]]
howmuch m n = if m > n then howmuch' n m else howmuch' m n

howmuch' :: Int -> Int -> [[String]]
howmuch' m n = [ ["M: " ++ show f, "B: " ++ show b, "C: " ++ show c]
              | f <- [m..n], 
                let maybeC = (f - 1) `mod` 9 == 0, 
                let maybeB = (f - 2) `mod` 7 == 0,
                maybeC && maybeB, 
                let c = (f - 1) `div` 9,
                let b = (f - 2) `div` 7
              ]

