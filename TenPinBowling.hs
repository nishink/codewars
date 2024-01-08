-- https://www.codewars.com/kata/5531abe4855bcc8d1f00004c
-- Ten-Pin Bowling

module TenPinBowling.JorgeVS.Kata where 

import Data.Char

bowlingScore :: String -> Int
bowlingScore myFrames = countScore $ map pins $ words myFrames

countScore :: [[Int]] -> Int
countScore [] = 0
countScore [lastFrame] = sum lastFrame
countScore (f:fs) = countScore fs +
  if f == [10] then 10 + next 2 fs -- strike
  else if sum f == 10 then 10 + next 1 fs -- spare
  else sum f

next :: Int -> [[Int]] -> Int
next n nss = sum $ take n $ concat nss

pins :: String -> [Int]
pins ('X':fs) = [10] ++ pins fs
pins [a,'/'] = [digitToInt a, 10 - (digitToInt a)]
pins [a,'/',b] = pins [a,'/'] ++ pins [b]
pins frame = map digitToInt frame

{-
ボウリングのルール

ストライクは、その時倒した１０ピン＋次の２投で倒したピンの数がポイントになる。
３連続ストライクなら、最初のストライクは３０点になるが、
２、３回目のストライクは、その次に倒したピンの数次第でポイントが不定になる。

スペアは、その時倒した１０ピン＋次の１投で倒したピンの数がポイントになる。
スペアの次がストライクなら２０点。

１０フレーム目でストライクまたはスペアを出した場合は、
上記のポイントは加算されず、倒したピンの数がポイントとなる。

さて、これをどうやって計算するか。

まずmyFrameをスペースで１０フレームに分割し、
個々のフレームの得点を計算することから考えてみる。

１〜９フレーム目は、
ストライクなら１０点＋次の２投分のボーナス。
スペアなら１０点＋次の１投分のボーナス
それ以外は倒したピンの数が点数。

１０フレーム目は、ボーナスなしで単に倒したピンの数が点数。



-}


