-- https://www.codewars.com/kata/5b1cd19fcd206af728000056
-- Which x for that sum?

module WhichX.Kata (solve) where

solve :: Double -> Double
solve m = binarySearch 0.0 1.0
  where
    f x = x / (1 - x) ^ 2
    binarySearch low high
      | high - low < 1e-13 = mid
      | f mid < m = binarySearch mid high
      | otherwise = binarySearch low mid
      where mid = (low + high) / 2