-- https://www.codewars.com/kata/536a155256eb459b8700077e
-- The Clockwise Spiral

module Spiral (createSpiral) where

import Data.List
-- import Data.List.Split

createSpiral :: Int -> [[Int]]
createSpiral n 
    | n <= 0 = []
    | otherwise = chunksOf n $ map (indexOf (snail $ chunksOf n [1..n*n])) [1..n*n]

snail :: [[Int]] -> [Int]
snail [] = []
snail (xs:xss) = xs ++ (snail . reverse . transpose) xss

indexOf :: [Int] -> Int -> Int
indexOf ns n = 1 + head (elemIndices n ns)

---
-- ローカルにData.List.Splitが入っていないのでchunksOfを自作。

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

{-
時計回りの数字列（二次元配列）を作る。
以前解いた「Snail」の応用。
https://www.codewars.com/kata/521c2db8ddc89b9b7a0000c1/haskell
snail関数はここのベストプラクティスから拝借。
-}