-- https://www.codewars.com/kata/59cf0ba5d751dffef300001f
-- Simple Fun #358: Vertical Histogram Of Letters

module Histogram where

import Data.Char ( isUpper )
import Data.List ( group, intercalate, intersperse, sort )

histogram :: String -> String
histogram s = let
    (charas, lens) = countChara s
    in if null lens then "" else let
       mlen = maximum lens
       graph = drawGraph mlen lens ++ [charas]
       in intercalate "\n" $ map (rtrim . intersperse ' ') graph

countChara :: String -> (String, [Int])
countChara s = let
    list = group $ sort $ filter isUpper s
    in (map head list, map length list)

drawGraph :: Int -> [Int] -> [String]
drawGraph 0 _ = []
drawGraph n ns = map (\x -> if x < n then ' ' else '*') ns : drawGraph (n-1) ns

rtrim :: String -> String
rtrim s = reverse $ dropWhile (==' ') $ reverse s

{-
入力文字列から大文字のみをフィルタ。
ソートしてグルーピングし、文字ごとの数を数える。
一番多い文字の数を基準にヒストグラムを書き始める。
一旦間のスペースは考えない。
文字数を減らしていって、文字の数以内なら*を、そうで無いならスペースを出力。
0まできたら文字自体を出力。
間にスペースを入れつつ、末尾のスペースを削ればできあがり。
-}