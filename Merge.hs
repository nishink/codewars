-- https://www.codewars.com/kata/52336a4436e0b095d8000093
-- MergeSort "merge" function

module Merge where

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


{-
マージソートのマージだけ
https://qiita.com/ymmy02/items/3fbab0ca3518ae19b7d6


-}