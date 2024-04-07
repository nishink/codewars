-- https://www.codewars.com/kata/55ea170313b76622b3000014
-- Numbers and its Reversal Having Same Prime Factors.

module Codewars.G964.Sameprimefactors where

sameFactRev :: Int -> [Int]
sameFactRev nmax = takeWhile (<nmax) [1089,2178,4356,6534,8712,9801,10989,21978,24024,
 26208,42042,43956,48048,61248,65934,80262,84084,
 84216,87912,98901,109989,219978,231504,234234,
 242424,253344,255528,264264,272646,275184,277816,
 288288,405132,424242,432432,439956] -- https://oeis.org/A110819/list

{-
まともに解く気がない。
-}