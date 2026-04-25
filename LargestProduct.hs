-- https://www.codewars.com/kata/529872bdd0f550a06b00026e
-- Largest product in a series

module LargestProduct where

import Data.Char

greatestProduct :: String -> Int
greatestProduct str = let
    headSeries = take 5 str
    tailSeries = drop 5 str
    firstProduct = mult headSeries
    in f headSeries tailSeries firstProduct
    where
        f _ [] p = p
        f (h:hs) (t:ts) p = let
            newH = hs ++ [t]
            newP = mult newH
            in if newP > p then f newH ts newP else f newH ts p

mult :: String -> Int
mult str = product $ map digitToInt str

{-
指定された桁の文字列で連続した5桁の最大の積を見つけるように、greatestProductメソッドを完成させます。
例えば、文字列"123834539327238239583"の5桁の連続した最大積は3240です。
入力文字列は常に5桁以上です。

思いつく解き方としてはこんな感じ。
最初の５桁を取得して掛け算する。この数を覚えておく。
最初の５桁のうちの最初の桁を削り、次の１桁を取得して掛け算し、
この数が前回の掛け算の結果と比較して大きければ残す。
これを繰り返す。
-}