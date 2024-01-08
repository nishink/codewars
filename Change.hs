-- https://www.codewars.com/kata/541af676b589989aed0009e7
-- Counting Change Combinations

module Change where

import Data.List

countChange :: Integer -> [Integer] -> Integer
countChange 0 _ = 1
countChange amount coins = count amount coins $ group $ sort coins

count amount coins gs = let
    cnt = reached amount gs
    ns = unreached amount gs
    ns2 = nexts amount coins ns
    in cnt + if null ns then 0 else count amount coins ns2

nexts amount coins gs = nub $ map sort [ c:g | g <- gs, c <- coins, sum (c:g) <= amount ]

reached amount ns = fromIntegral $ length $ filter (== amount) (map sum ns)

unreached amount ns = [ n | n <- ns, sum n <= amount ]

{-
問題：
両替の方法が何通りあるか。
１つ目の引数が合計金額、２つ目の引数がコインの価値。
  countChange 4 [1,2] -- => 3
  countChange 10 [5,2,3] -- => 4
  countChange 11 [5,7] -- => 0
考え方：
　コインを１つずつ取り出して、合計金額未満ならもう１つコインを取り出す。
　合計金額と一致したものがあればカウントを増やす。
　合計金額を超えたら終了。
　ソートして重複を削除。



-}