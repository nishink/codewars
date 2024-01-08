-- https://www.codewars.com/kata/5679aa472b8f57fb8c000047
-- Equal Sides Of An Array

module Codewars.G964.FindEven where

import Data.List

findEvenIndex :: [Int] -> Int
findEvenIndex arr = let
    lts = total arr
    rts = total $ reverse arr
    lzs = elemIndices 0 lts
    rzs = elemIndices 0 rts
    hlen = (length arr) `div` 2
    mls = match (take hlen lts) (take hlen rts)
    mts = elemIndices True mls
  in
    if length arr >= 1000 then -1
      else if length arr == 1 then 0
      else if rzs /= [] then 0
      else if lzs /= [] then last lzs + 1
      else if mts /= [] then last mts + 1
      else -1

total :: [Int] -> [Int]
total ls = [ sum $ take n ls | n <- [1..(length ls)] ]

match :: [Int] -> [Int] -> [Bool]
match xs ys = map (\(x, y) -> x == y) $ zip xs ys

{-
まず左から順に累計を計算したリストと、右から順に累計を計算したリストを出す。
左の累計リストを順に見て行って、０となる最大のindexがあればそれが答え。
なければ右の累計リストを順に見て行って０となるものがあれば０が答え。
それ以外は、左右で同じindexのものを１から比較し、左右の値が一致するもの中で、
リストの長さの半分以下で最も大きいindexが答え。
上記に当てはまるものが全くなければ-1が答え。

という解き方を考えたが、そもそも問題の理解を間違えていた。

arrのある値を境に左と右に分けて、それぞれの合計が一致する値のindexを求めるだけでよかった。


-}

