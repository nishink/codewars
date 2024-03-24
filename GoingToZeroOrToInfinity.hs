-- https://www.codewars.com/kata/55a29405bc7d2efaff00007c
-- Going to zero or to infinity?

module Codewars.Kata.Suite1 where

import Data.Ratio

going :: Integer -> Double
going n = fromIntegral (round (u' (fromIntegral n) * 1000000)) / 1000000

-- 問題文ほぼそのままの式
u n = (sum $ map factorial [1..n]) / (factorial n)

-- 変形して少し単純化してみる
u' n = 1 + sum [ 1 / (product [m..n]) | m <- [2..n]]

-- 分数を使ってみる
u'' :: Integer -> Float
u'' n = let
    x = (sum $ map factorial [1..n]) % (factorial n)
    in fromIntegral (numerator x) / fromIntegral (denominator x)

-- 階乗
factorial n = product [1..n]

{-
ゼロに行くか、無限に行くか。

u1 = (1 / 1!) * (1!)
u2 = (1 / 2!) * (1! + 2!)
u3 = (1 / 3!) * (1! + 2! + 3!)
...
un = (1 / n!) * (1! + 2! + 3! + ... + n!)

unの値はゼロに近づくのか、無限大に行くのかを求める。

1! = 1
2! = 1 * 2 = 2
3! = 1 * 2 * 3 = 6
4! = 1 * 2 * 3 * 4 = 24

u1 = (1 / 1!) * (1!) = (1 / 1) * 1 = 1
u2 = (1 / 2!) * (1! + 2!) = (1 / 2) * (1 + 2) = 3 / 2
u3 = (1 / 3!) * (1! + 2! + 3!) = (1 / 6) * (1 + 2 + 6) = 9 / 6 = 3 / 2
u4 = (1 / 4!) * (1! + 2! + 3! + 4!) = 1 / 24 * (1 + 2 + 6 + 24) = 33 / 24 = 11 / 8

式を単純化してみる。

un = (1 / n!) * (1! + 2! + 3! + ... + n!)
   = (1! / n! + 2! / n! + 3! / n! + ... + 1)
   = (1 / (2*3*...*n) + 1 / (3*4*...*n) + ... + 1 / n + 1)

問題文に、小数点以下６桁までとあるのを見落としていたので、
n = 7がどうしてもサンプルテストどおりにならずにしばらく悩んでしまった。
最終的にはu'で変形した式の形で解けた。
分数を使う手も考えたが、n = 200の時にNaNになってしまった。

-}