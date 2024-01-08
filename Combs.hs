-- https://www.codewars.com/kata/58898b50b832f8046a0000ec
-- Simple Fun #53: Combs

module Combs (combs) where

combs :: String -> String -> Int
combs comb1 comb2 = let
    len1 = length comb1
    len2 = length comb2
    ext1 = replicate len2 '.'
    ext2 = replicate len1 '.'
    comb1' = ext1 ++ comb1 ++ ext1
    comb2' = ext2 ++ comb2 ++ ext2
    in minimum (
        [ s | i <- [0..len2], let (b, s) = fit (drop i comb1') comb2', b ] ++
        [ s | i <- [0..len1], let (b, s) = fit comb1' (drop i comb2'), b ]
    )

fit :: String -> String -> (Bool, Int)
fit comb1 comb2 = let
    isFit = notElem ('*','*') $ zip comb1 comb2
    in (isFit, size comb1 comb2)

size :: String -> String -> Int
size comb1 comb2 =
    length $ trim ('.','.') $ zip comb1 comb2
    
trim :: Eq a => a -> [a] -> [a]
trim element target =
    reverse $ dropWhile (==element) $ reverse $ dropWhile (==element) target

{-
For comb1 = "*..*" and comb2 = "*.*", the output should be 5

２つの櫛を重ね合わせて置いた時、欠けた歯が噛み合うようにすることで、
短い長さで配置できるかどうかを確認する。

櫛の長さが最大で１０なので、まずは総当たりを試す。
櫛１＋櫛２、もしくは櫛２＋櫛１が最大の長さなので、
櫛１の前に櫛２と同じ長さの下駄を履かせるところから、
櫛２の前に櫛１と同じ長さの下駄を履かせるところまでを試す。
具体的には以下の通り。

...*..*
*.*....

..*..*
*.*...

.*..*
*.*..

*..*
*.*.

*..*
.*.*

*..*.
..*.*

*..*..
...*.*

*..*...
....*.*


...*..*...
....*.*....
1,0 2,0 3,0 
0,0 
0,1 0,2 0,3 0,4
-}
