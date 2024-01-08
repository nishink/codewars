-- https://www.codewars.com/kata/55e1990978c60e5052000011
-- Merge in 2048

module Haskell.Codewars.Merge2048 where


merge :: [Int] -> [Int]
merge array = m $ slide array
  where
    m [] = []
    m [x] = [x]
    m (x:y:xs) = if x == y then (x+y) : m xs ++ [0] else x : m (y:xs)

slide [] = []
slide [x] = [x]
slide (x:xs) = if x == 0 then slide xs ++ [0] else x : slide xs
