-- https://www.codewars.com/kata/60a1aac7d5a5fc0046c89651
-- Last Survivors Ep.2

module LastSurvivors (lastSurvivors) where

import Data.List

lastSurvivors :: String -> String
lastSurvivors s = let
  s' = operation (sort s)
  in if s == s' then s else lastSurvivors s'

transform :: Char -> Char
transform x
  | x == 'z' = 'a'
  | otherwise = succ x

operation :: String -> String
operation (x:y:zs)
  | x == y    = transform x : operation zs
  | otherwise = x : operation (y:zs)
operation x = x

