-- https://www.codewars.com/kata/57ce3d87f5f4ef590000001b
-- T.T.T.60: Break all the rules

module BreakAllTheRules (breakRules) where

import Data.List

breakRules :: [(Int,Int)] -> [[Int]]
breakRules rules = let
    perms = permutations $ toNumbers rules
    in nub $ sort $ f rules perms
    where
        f [] perms = perms
        f (x:xs) perms = f xs (filter (checkRule x) perms)

toNumbers :: [(Int,Int)] -> [Int]
toNumbers rules = concat [ [fst r, snd r] | r <- rules ]

checkRule :: (Int,Int) -> [Int] -> Bool
checkRule rule nums = let
    (_, ns) = break (== fst rule) nums
    in null (elemIndices (snd rule) (tail ns)) 

{-
ルールに従わない組み合わせを抽出する。

(1,2)というルールがあった時、組み合わせは[1,2],[2,1]の２通りだが、
[1,2]は除外されるので、[2,1]だけが残る。

まず、ルールに含まれる値だけを抽出したリストを作る。
そのリストの全ての組み合わせを網羅したリストを作り、
最後にルールを１つずつ見ていって条件に合うものを除外していく。

-}