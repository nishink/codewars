-- https://www.codewars.com/kata/5b997b066c77d521880001bd
-- Shuffle It Up

module Shuffle where

allPermuted :: Integer -> Integer
allPermuted 0 = 0
allPermuted 1 = 0
allPermuted 2 = 1
allPermuted n = (n-1) * (permList !! fromIntegral (n-1) + permList !! fromIntegral (n-2))

permList :: [Integer]
permList = map allPermuted [0..]

{-
順列の数を数える。
ただし、元の順列と同じ位置に同じ数字が来ない組み合わせだけを数える。
例：
[1] -> 0
[1,2] -> 1
(2,1)
[1,2,3] -> 2
(2,3,1)
(3,1,2)
[1,2,3,4] -> 9
[1,2,3,4,5] -> 
(2,3,4,5,1)
:

これを考えていくと大変なので、解き方を調べる。

https://www.geeksforgeeks.org/count-derangements-permutation-such-that-no-element-appears-in-its-original-position/

結論としては、

allPermuted n = (n-1) * (allPermuted (n-1) + allPermuted (n-2))
らしい。

さて、これをそのまま適用してみたが、見事にタイムアウトになった。
なので、先に無限リスト(permList)を作ってからallPermutedを適用する形をとった。

-}