-- https://www.codewars.com/kata/5426006a60d777c556001aad
-- Dithering

module Dithering (dithering) where

import Data.List

dithering :: Word -> Word -> [(Word,Word)]
dithering width height = filter (\(x,y) -> x < width && y < height) $ dither $ power $ max width height

dither 1 = [(0,0),(1,1),(1,0),(0,1)]
dither n = concat $ transpose $ map (wide . double) $ dither (n-1)

double (x,y) = (x*2,y*2)
wide (x,y) = [(x,y),(x+1,y+1),(x+1,y),(x,y+1)]
power x = f 1 where f n = if x <= 2^n then n else f (n+1)

{-
ディザリング
2x2の場合、
1 3
4 2
の順
4x4の場合、
全体を４分割して同じパターンを繰り返す。

2x2から4x4を作るには
まず二倍に広げる

 1 _ | 3 _
 _ _ | _ _
-----+-----
 4 _ | 2 _
 _ _ | _ _

分割したエリアごとに、以下のような値を設定する。

 (x+0)  (x+8)
 (x+12) (x+4)

4を加算しているのは、4x4が2x2の4倍だから。
これが8x8になるときは16を加算する。
8x8が2x2の16倍なので。

座標で表すと、まず2x2は
[(0,0),(1,1),(1,0),(0,1)]
これを２倍して
[(0,0),(2,2),(2,0),(0,2)]
領地を広げる
[
 [(0,0),(1,1),(1,0),(0,1)],
 [(2,2),(3,3),(3,2),(2,3)],
 [(2,0),(3,1),(3,0),(2,1)],
 [(0,2),(1,3),(1,2),(0,3)],
]
行と列を入れ替えるとちょうど4x4になる。

-}