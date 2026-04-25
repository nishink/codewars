-- https://www.codewars.com/kata/55f2b110f61eb01779000053
-- Beginner Series #3 Sum of Numbers

module GetSum where

getSum :: Int -> Int -> Int
getSum a b = if a < b then sum [a..b] else sum [b..a]