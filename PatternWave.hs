-- https://www.codewars.com/kata/56e67d6166d442121800074c
-- ■□ Pattern □■ : Wave

module PatternWave where

import Data.List (intercalate)

draw :: [Int] -> String
draw [] = ""
draw pattern = intercalate "\n" $ reverse $ wave (maximum pattern) []
    where
        wave :: Int -> [String] -> [String]
        wave 0 acc = acc
        wave n acc = wave (n-1) (waveLine n pattern : acc)
        waveLine :: Int -> [Int] -> String
        waveLine n pattern = map (\x -> if x >= n then '■' else '□') pattern

{-
指定したパターンに合わせて波型の図形を表示する。
パターンは、1以上の整数のリストで、各整数は波の高さを表す。

例:
draw [1,2,3] == "□□■\n□■■\n■■■"

解き方：
1. パターンの最大値を求める
2. パターンの最大値を基準に、各行の文字列を生成する
3. 各行の文字列を連結して、最終的な図形を生成する



-}