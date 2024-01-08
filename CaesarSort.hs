-- https://www.codewars.com/kata/5483b69b48cf540cfc000119/haskell
-- Caesar Cipher Sorting

module CaesarSort where

import Data.Char
import Data.List

caesarSort :: [String] -> [[String]]
caesarSort ss =
    map (map snd) $ groupBy (\x y -> fst x == fst y) sort [ (caesarValue s, s) | s <- ss ]

caesarValue :: String -> String
caesarValue "" = ""
caesarValue s@(x:xs) = let
    n = ord x - ord 'a'
    f n c = if ord c - n < ord 'a' then chr (ord c - n + 26) else chr (ord c - n)
    in map (f n) s

{-
シーザー暗号ソート。
シーザー暗号というのは、アルファベットをシフトして生成する暗号のこと。

シフトすれば同じ文字列になるものを同じグループになるようにソートしたい。
なので、「シーザー暗号値」を計算する。
与えられた文字列の先頭が「a」になるようにシフトした値を「シーザー暗号値」とし、
与えられた文字列全ての「シーザー暗号値」を求める。
あとはそれでグルーピングすればいい。


-}