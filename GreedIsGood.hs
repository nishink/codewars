-- https://www.codewars.com/kata/5270d0d18625160ada0000e4
-- Greed is Good

module Greed (score) where

score :: [Int] -> Int
score dice = 
  let counts = map (\x -> length $ filter (== x) dice) [1..6]
      tripletScore = case counts of
        (c1:c2:c3:c4:c5:c6:_) -> (if c1 >= 3 then 1000 else 0)
                              + (if c2 >= 3 then 200 else 0)
                              + (if c3 >= 3 then 300 else 0)
                              + (if c4 >= 3 then 400 else 0)
                              + (if c5 >= 3 then 500 else 0)
                              + (if c6 >= 3 then 600 else 0)
        _ -> 0
      singleScore = case counts of
        (c1:c2:c3:c4:c5:c6:_) -> (if c1 < 3 then c1 * 100 else (c1 - 3) * 100)
                              + (if c5 < 3 then c5 * 50 else (c5 - 3) * 50)
        _ -> 0
  in tripletScore + singleScore
{-
DESCRIPTION:

Greed is a dice game played with five six-sided dice. Your mission, should you choose to accept it, is to score a throw according to these rules. You will always be given an array with five six-sided dice values.

 Three 1's => 1000 points
 Three 6's =>  600 points
 Three 5's =>  500 points
 Three 4's =>  400 points
 Three 3's =>  300 points
 Three 2's =>  200 points
 One   1   =>  100 points
 One   5   =>   50 point
Each of 5 dice can only be counted once in each roll. For example, a given "5" can only count as part of a triplet (contributing to the 500 points) or as a single 50 points, but not both in the same roll.

Example scoring

 Throw       Score
 ---------   ------------------
 5 1 3 4 1   250:  50 (for the 5) + 2 * 100 (for the 1s)
 1 1 1 3 1   1100: 1000 (for three 1s) + 100 (for the other 1)
 2 4 4 5 4   450:  400 (for three 4s) + 50 (for the 5)
-}