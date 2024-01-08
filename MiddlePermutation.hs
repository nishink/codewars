-- https://www.codewars.com/kata/58ad317d1541651a740000c5
-- Simple Fun #159: Middle Permutation

module MiddlePermutation.JorgeVS.Kata where

import Data.List

middlePermutation :: String -> String
middlePermutation myString = let
    len = length myString
    idx = (len - 1) `div` 2
    ss = sort myString
    mdl = ss !! idx
    rem = filter (mdl/=) ss
    in if even len then mdl : reverse rem else mdl : middlePermutation rem

{-
いくつかの結果を見ていくと、なんとなく規則性がありそうだったが、
面倒なのでググったら考え方が出てきた。

https://codereview.stackexchange.com/questions/163292/finding-the-middle-permutation
1.与えられた文字列をソートする
2.文字列の長さが偶数の場合、ほぼ中間にある文字を取り出し、次のように書き換える。
　'ほぼ中間文字' : reverse "残りの文字"
3.文字列の長さが奇数の場合、中間にある文字を取り出し、次のように書き換える。
　'中間文字' : middlePermutation "残りの文字"
-}
