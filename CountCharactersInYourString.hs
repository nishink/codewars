-- https://www.codewars.com/kata/52efefcbcdf57161d4000091
-- Count characters in your string

module Count (count) where

import Data.List (group, sort)

count :: String -> [(Char,Int)]
count = map (\x -> (head x, length x)) . group . sort

