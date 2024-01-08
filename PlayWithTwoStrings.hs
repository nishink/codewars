-- https://www.codewars.com/kata/56c30ad8585d9ab99b000c54
-- Play with two Strings

module Play (work) where

import Data.Char

work :: String -> String -> String
work a b = rule a b ++ rule b a

rule :: String -> String -> String
rule as "" = as
rule as (b:bs) = rule [ if toLower a == toLower b then swapCase a else a | a <- as ] bs

swapCase :: Char -> Char
swapCase c
    | isLower c = toUpper c
    | otherwise = toLower c

