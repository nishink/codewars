-- https://www.codewars.com/kata/564057bc348c7200bd0000ff
-- A Rule of Divisibility by 13

module Codewars.G964.Thirteen where

import Data.Char

thirt :: Integer -> Integer
thirt n = let
    digits = map (fromIntegral . digitToInt) $ reverse $ show n
    calc = sum $ map (uncurry (*)) $ zip digits $ concat $ repeat [1,10,9,12,3,4]
    in if calc == n then n else thirt calc

