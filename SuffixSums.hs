-- https://www.codewars.com/kata/590938089ff3d186cb00004c
-- Simple Fun #237: Suffix Sums

module SuffixSums (suffixSums) where

suffixSums :: [Int] -> [Int]
suffixSums = scanr1 (+)

