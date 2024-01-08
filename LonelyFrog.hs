-- https://www.codewars.com/kata/5954b48ad8e0053403000040
-- Simple Fun #336: Lonely Frog

module LonelyFrog (jumpTo) where

jumpTo :: Int -> Int
jumpTo 0 = 0
jumpTo n = let
    (h, t) = break (\x -> x >= abs n && even (x - n)) total
  in length h + 1

total :: [Int]
total = [ (n * (n + 1)) `div` 2 | n <- [1..] ]

