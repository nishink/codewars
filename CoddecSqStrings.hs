-- https://www.codewars.com/kata/56fcc393c5957c666900024d
-- Coding with Squared Strings

module Codewars.G964.CoddecSqStrings where

import Data.List
import Data.List.Split

code :: [Char] -> [Char]
code s = intercalate "\n" $ map reverse $ transpose $ divvy n n text
    where
        n = ceiling $ sqrt $ fromIntegral $ length s
        text = s ++ replicate (n * n - length s) '\v'

decode :: [Char] -> [Char]
decode s = filter (/='\v') $ concat $ transpose $ map reverse $ lines s


{-
t = "I.was.going.fishing.that.morning.at.ten.o'clock"
code(t) -> "c.nhsoI\nltiahi.\noentinw\ncng.nga\nk..mg.s\n\voao.f.\n\v'trtig"
decode(code(t)) == "I.was.going.fishing.that.morning.at.ten.o'clock"

"c.nhsoI\n
 ltiahi.\n
 oentinw\n
 cng.nga\n
 k..mg.s\n
\voao.f.\n
\v'trtig"

n * nの行列になるようにした上で、
横書きを縦書きに変換し、足りない部分を「\v」で埋める。

n = ceiling $ sqrt $ length s

text = s ++ replicate (n * n - length s) '\v'

code s = unlines $ map reverse $ transpose $ divvy n n s

decode s = filter (=='\v') concat $ transpose $ map reverse $ lines s

-}
