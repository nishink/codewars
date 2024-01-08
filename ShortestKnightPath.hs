-- https://www.codewars.com/kata/549ee8b47111a81214000941
-- Shortest Knight Path

module ShortestKnightPath.Kata (knight) where

import Data.Char

knight :: String -> String -> Int
knight start goal = f [[an2pos start]] (an2pos goal)
    where
        f ss g = let
            ns = [ n:s | s <- ss, n <- nexts (head s), notElem n s ]
            in if elem g (map head ns) then length (head ns) - 1 else f ns g

an2pos :: String -> (Int,Int)
an2pos [h,v] = (ord h - ord 'a' + 1, digitToInt v)

nexts :: (Int,Int) -> [(Int,Int)]
nexts (x,y) = let
    ns = [(x+1,y+2),(x+1,y-2),(x+2,y+1),(x+2,y-1),(x-1,y+2),(x-1,y-2),(x-2,y+1),(x-2,y-1)]
    in filter (\(a,b) -> a >= 1 && b >= 1 && a <= 8 && b <= 8) ns


{-
チェス盤上の２地点において、ナイトが移動するのに何手必要かを求める。
チェス盤上の地点は、横がa~h、縦が1~8で表す。

スタート地点から次に移動できるところを辿っていって、
最初にゴール地点に辿り着いたパスの長さを求めればいい。

計算しやすいように、"a1"->(1,1)に変換する関数と、
次に移動する先の候補を取得する関数を作ってみる。

最初はスタート地点のみをリストに入れておき、
スタート地点から移動できる候補のリストを取得する。
スタート地点＋移動先のリストを作り、以降繰り返す。
移動先リストにゴールが含まれていたら終了。


-}