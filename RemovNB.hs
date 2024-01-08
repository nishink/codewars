-- https://www.codewars.com/kata/5547cc7dcad755e480000004
-- Is my friend cheating?

module Codewars.Kata.RemovNB where
      
removNb :: Integer-> [(Integer, Integer)]
removNb n = [ (a, b) |
             a <- [1..n],
             let b' = (n * (n + 1) `div` 2) - a,
             b' `mod` (a + 1) == 0,
             let b = b' `div` (a + 1),
             b <= n ]

{-
1からnの数列から２つの数を取り除く。
取り除いた数の積が、残った数の和に等しくなるような
２つの数のペアを返す。

1からnの和は、n(n+1)/2で表せる。
取り除いた数の積が、残った数の和に等しくなるというのを式で表すと、
n(n+1)/2-a-b=a*b
となる。
式を変形する。
a*b+a=n(n+1)/2-b
a(b+1)=n(n+1)/2-b
a={n(n+1)/2-b}/(b+1)
b={n(n+1)/2-a}/(a+1)

-}

