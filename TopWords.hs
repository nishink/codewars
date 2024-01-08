-- https://www.codewars.com/kata/51e056fe544cf36c410000fb
-- Most frequently used words in a text

module TopWords (top3) where

import Data.Char ( isUpper, toLower )
import Data.List ( group, sort, sortOn )
import Data.Ord ( Down(Down) )

top3 :: [Char] -> [[Char]]
top3 s = let
    wordGroup = group $ sort $ filter (any (/= '\'')) $ words $ map adjust s
    in map head $ take 3 $ sortOn (Down . length) wordGroup

adjust :: Char -> Char
adjust c
    | isUpper c = toLower c
    | c `notElem` ('\'':['a'..'z']) = ' '
    | otherwise = c

{-
頻出する単語トップ３を抽出する。

まず対象の文字列を調節する。
大文字は小文字に、
アルファベットと'(シングルクオート)以外はスペースに。

単語ごとに分割したら、ソートしてグルーピングする。
数の多い順でさらにソートして、最初の３つを取り出す。
-}