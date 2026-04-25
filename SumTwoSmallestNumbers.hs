-- https://www.codewars.com/kata/558fc85d8fd1938afb000014
-- Sum of two lowest positive integers

module Sums (sumTwoSmallestNumbers) where

import Data.List (sort)

sumTwoSmallestNumbers :: [Int] -> Int
sumTwoSmallestNumbers nums = let (x:y:_) = sort nums in x + y

{-
配列として4つ以上の正の整数が与えられたとき、
最も小さい2つの正の数の合計を返す関数を作成してください。
小数や0以下の整数が渡されることはありません。

例えば、[19, 5, 42, 2, 77] が渡された場合、出力は 7 となります。

[10, 343445353, 3453445, 3453545353453] の場合は 3453455 を返すべきです。
-}