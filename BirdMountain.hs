-- https://www.codewars.com/kata/5c09ccc9b48e912946000157
-- Bird Mountain

module BirdMountain (peakHeight) where

import Debug.Trace

peakHeight :: [[Char]] -> Int
peakHeight area = countHeight area 0
    where
        countHeight a h
            | all (==' ') (concat a) = h
            | otherwise = countHeight (landfill a) (h+1)

landfill :: [[Char]] -> [[Char]]
landfill area = [ [ if isEdge area x y then ' ' else '^' | x <- [0..westeast-1] ] | y <- [0..northsouth-1] ]
    where
        northsouth = length area
        westeast = length $ head area
        isEdge area x y = 
            (' ' == area !! y !! x) || 
            (if y == 0 then True else if ' ' == area !! (y-1) !! x then True else False) || 
            (if y == northsouth-1 then True else if ' ' == area !! (y+1) !! x then True else False) || 
            (if x == 0 then True else if ' ' == area !! y !! (x-1) then True else False) || 
            (if x == westeast-1 then True else if ' ' == area !! y !! (x+1) then True else False)

{-
鳥瞰図
山の高さを測る問題。

これも、LandPerimeterと同様に解けそう。

隣が山でない箇所を消していって、全部消えた時の消した回数が高さになる。

-}