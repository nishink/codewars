-- https://www.codewars.com/kata/54d81488b981293527000c8f
-- Sum of Pairs

module Pairs (sumPairs) where

import Data.Set as S

sumPairs :: [Int] -> Int -> Maybe (Int,Int)
sumPairs (x:xs) s = searchPair (S.singleton x) xs
    where
        searchPair _ [] = Nothing
        searchPair st (y:ys) = let
            z = s - y
            in  if S.member z st then Just (z, y)
                else searchPair (S.insert y st) ys

{-
整数のリストと単一の合計値が与えられた場合、最初の2つの値（左から解析してください）を、
合計を形成するために合計する外観の順序で返します。

必要な合計を持つ2つ以上のペアがある場合、2番目の要素のインデックスが最小のペアが解です。

負の数字と重複した数字は表示できます。

注：10,000,000要素以上の長さのテストリストもあります。
コードがタイムアウトしないように注意してください。

解き方：
おそらくすべての組み合わせを試す方法はタイムアウトしてしまう。
したがって、最初にペアが見つかった時点で終了するようにしたい。

Map型の変数が使えるなら、リストを１つずつ読み込んでいき、
見つかった時点で終了することができるだろう。
リストの最初の要素をMapに入れ、２つ目の要素とペアになる値がMapにあるかチェック。
あればその時点で終了、なければ２つ目の要素をMapに入れて繰り返し。
Mapを使うのは重複を排除するため。

-}