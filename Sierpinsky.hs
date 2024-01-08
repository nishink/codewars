-- https://www.codewars.com/kata/53ea3ad17b5dfe1946000278
-- Sierpinski's Gasket

module Sierpinsky where

import Data.List

sierpinsky :: Integral a => a -> String
sierpinsky n = 
    intercalate "\n" (map rtrim $ lines $ f n)

f 0 = "L "
f n =
    unlines (map padSpace $ lines $ f (n-1)) ++
    unlines (map twice $ lines $ f (n-1))

padSpace s = s ++ replicate (length s) ' '

twice s = s ++ s

rtrim s = let
    (_, r) = span (==' ') $ reverse s
    in reverse r

{-
シェルピンスキーのギャスケットは、フラクタル図形の一種で、
同じ形状の図形を繰り返すことで生成する図形のこと。
この問題では「L」を繰り返すことで図形を生成する。

基本図形は "L " として、
上段は同じ長さのスペースで埋め、
下段は二倍に増やす。
これを繰り返す。
最終的な出力の際、余分な右側のスペースは削る。

f 0 = "L "
g 0 = "  "

f 1 = f 0 + g 0
    + f 0 + f 0
g 1 = g 0 + g 0
    + g 0 + g 0

f 2 = f 1 + g 1
    + f 1 + f 1
g 2 = g 1 + g 1
    + g 1 + g 1

：

以下はバイナリでやろうと模索した時のメモ。

0
1

00
01
10
11

1000:8
1100:12
1010:10
1111:15

0000000000000001:1
0000000000000011:3
0000000000000101:5
0000000000001111:F
0000000000010001:11
0000000000110011:33
0000000001010101:55
0000000011111111:FF
0000000100000001:0101
0000001100000011:0303
0000010100000101:0505
0000111100001111:0F0F
0001000100010001:1111
-}