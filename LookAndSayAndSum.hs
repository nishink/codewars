-- https://www.codewars.com/kata/5922828c80a27c049c000078
-- Simple Fun #299: Look And Say And Sum

module Haskell.Codewars.LookAndSayAndSum where

import Data.Char
import Data.List

lookAndSaySum :: Int -> Int
lookAndSaySum n = sum $ map digitToInt $ show $ lookAndSay $ fromIntegral n

lookAndSay :: Integer -> Integer
lookAndSay 1 = 1
lookAndSay n =
    read $ concat [ (show $ length elm) ++ [head elm] | elm <- group $ show $ lookAndSay (n-1) ]

{-
https://ja.wikipedia.org/wiki/読み上げ数列

-}
