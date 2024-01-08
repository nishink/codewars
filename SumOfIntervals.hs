-- https://www.codewars.com/kata/52b7ed099cdc285c300001cd
-- Sum of Intervals

module SumOfIntervals (sumOfIntervals) where

import Data.List (nub)

sumOfIntervals :: [(Int, Int)] -> Int
sumOfIntervals = negate . sum . map (uncurry (-)) . distinct

-- 重なりの判定
overlapped :: (Int, Int) -> (Int, Int) -> Bool
overlapped (a, b) (c, d) = not (b < c || d < a)

-- 重なっていたら合体する
overlap :: (Int, Int) -> (Int, Int) -> (Int, Int)
overlap (a, b) (c, d) =
    if overlapped (a, b) (c, d) then (min a c, max b d) else (c, d)

-- 重なりの削除
distinct :: [(Int, Int)] -> [(Int, Int)]
distinct [] = []
distinct (x:xs) = 
    if any (overlapped x) xs 
        then distinct (map (overlap x) xs)
        else x : distinct xs
{-
間隔の合計を求める。
ただし、間隔が重なる場合は合体させた長さで計算する。


---
重なるパターン
a   b
  c    d

  a   b
c   d

  a   b
c        d

a         b
   c   d

--- 
重ならないパターン
a b 
    c d
    a b
c d


-}

