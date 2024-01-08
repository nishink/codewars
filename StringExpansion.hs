-- https://www.codewars.com/kata/5a793fdbfd8c06d07f0000d5
-- Simple string expansion

module StringExpansion where 

import Data.Char

solve :: [Char] -> [Char]
solve [] = []
solve (x:xs)
    | isAlpha x  = x : solve xs
    | isNumber x = concat $ replicate (digitToInt x) (solve (init $ tail xs))
    | otherwise  = solve xs

{-
      solve "3(ab)" `shouldBe` "ababab"
      solve "2(a3(b))" `shouldBe` "abbbabbb"
      solve "3(b(2(c)))" `shouldBe` "bccbccbcc"
      solve "k(a3(b(a2(c))))" `shouldBe` "kabaccbaccbacc"

数字が来た場合、その次の括弧を数字の回数繰り返す。


-}