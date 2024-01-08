-- https://www.codewars.com/kata/571249a0159cde165f00088a
-- Coding 3min : Count animals

module Kata.CountAnimals (sc) where

import Data.List

--import Preloaded.CountAnimals (names)
names = ["dog","cat","bat","cock","cow","pig","fox","ant","bird","lion","wolf","deer","bear","frog","hen","mole","duck","goat"]

sc :: String -> Int
sc str = count [str] 0
    where
        count xs i = let
            result = [ d | s <- xs, n <- names, let (b, d) = getName s n, b ]
            in if null result then i else count result (i+1)

getName :: String -> String -> (Bool, String)
getName str name = 
    if length str < length name then (False, "")
        else if length str == length diff + length name then (True, diff)
        else (False, "")
    where diff = str \\ name

{-
与えられた文字列から何匹の動物を取り出すことができるか。

まず、与えられた文字列に対して、namesにある動物を取り出せるか一通り試す。
・与えられた文字列が、動物の名前より短い場合は取り出せない。
・与えられた文字列から、動物の名前との差集合(\\)をとり、
　その結果の文字列の長さが、元の文字列から動物の名前の長さを引いた長さなら取り出せる。
　そうでなければ取り出せない。
・取り出した後の文字列に対して、同じ操作を繰り返す。

-}