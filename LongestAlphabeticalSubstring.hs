-- https://www.codewars.com/kata/5a7f58c00025e917f30000f1
-- Longest alphabetical substring

module LongestAlphabeticalSubstring (longest) where

import Data.List

longest :: String -> String
longest str = head $ sortBy (\a b -> compare (length b) (length a)) $ f str []
    where
        f [] ss = [ss]
        f [x] ss = [ss++[x]]
        f (x:y:xs) ss = if x <= y then f (y:xs) (ss++[x]) else (ss++[x]) : f (y:xs) [] 

{-
最長の部分文字列を抽出する。
部分文字列はアルファベットで昇順である必要があるため、
まずアルファベットが降順になったところで分割し、長さでソートして最長のものを取り出す。

-}