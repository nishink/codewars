-- https://www.codewars.com/kata/5a4a391ad8e145cdee0000c4
-- String subpattern recognition II

module HasSubPatternII where

import Data.List

hasSubpattern :: String -> Bool
hasSubpattern s = let
    counts = map length . group . sort $ s
    gcdCount = foldr1 gcd counts
    in gcdCount >= 2

{-
説明：
ある文字列がサブパターンを持つ場合にtrueを返す。
ただし、文字列はシャッフルされているかもしれない。
サブパターンは、元の文字列より短く、２回以上登場する。

例：
"a"    --> false //no repeated shorter sub-pattern, just one character
"aaaa" --> true  //just one character repeated
"abcd" --> false //no repetitions
"babababababababa" --> true //repeated "ba"
"bbabbaaabbaaaabb" --> true //same as above, just shuffled

解き方：
文字列をソートして同じ文字ごとの出現回数を数える
その出現回数の最大公約数（GCD）が 2 以上なら true
そうでなければ false


-}