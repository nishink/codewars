-- https://www.codewars.com/kata/5536a85b6ed4ee5a78000035
-- Help your granny!

module Codewars.Kata.Granny where

import Data.Maybe

tour :: [String] -> [(String, String)] -> [(String, Double)] -> Integer
tour friends fTowns distTable = calc friends "" 0.0
    where
        calc :: [String] -> String -> Double -> Integer
        calc [] lastFriend acc = floor (acc + getDistance lastFriend fTowns distTable)
        calc (x:xs) lastFriend acc = let
            nDist = getDistance x fTowns distTable
            cDist = getDistance lastFriend fTowns distTable
            dist = if lastFriend == "" then nDist else nextDistance cDist nDist 
            in if nDist == 0.0 then calc xs lastFriend acc else calc xs x (acc + dist)

nextDistance :: Double -> Double -> Double
nextDistance current next = sqrt (next^2 - current^2)

getValue :: String -> [(String, a)] -> Maybe a
getValue key list = let
    elem = filter (\x -> fst x == key) list
    in if null elem then Nothing else Just (snd $ head elem)

getDistance :: String -> [(String, String)] -> [(String, Double)] -> Double
getDistance friend fTowns distTable = let
    d = f friend
    in if d == Nothing then 0.0 else fromJust d
    where
        f friend = do
            town <- getValue friend fTowns
            dist <- getValue town distTable
            return dist

friends = ["A1", "A2", "A3", "A4", "A5"]
fTowns = [("A1", "X1"), ("A2", "X2"), ("A3", "X3"), ("A4", "X4")]
distTable = [("X1", 100.0), ("X2", 200.0), ("X3", 250.0), ("X4", 300.0)]




{-
おばあちゃんが友人を順に訪問して戻ってくる距離を計算する。
入力として与えられるのは以下。
・訪問する友人とその順序（友人リスト）
・友人とその住所（住所リスト）
・おばあちゃんの家から友人の住所までの距離（距離リスト）

ただしこれだけだと、友人の住所間の距離がわからない。
そこで条件として以下がある。
・おばあちゃんの家X0と、友人の住所X1、次に訪問する友人の住所X2は
　直角三角形の関係にある。

従って、X0-1が100、X0-2に200と与えられた時、
X1-2は sqrt ( 200^2 - 100^2 ) で求められる。

また住所がわからない友人は訪問しない。

まずはこの条件で例題を解くプログラムを考えてみよう。

友人の一人を友人リストから取り出す。
その友人の住所を住所リストから取り出す。
　この時取り出せない場合はその友人を訪問しない。
訪問先の住所から、おばあちゃんの家からの距離を距離リストから取り出す。
　現在地がおばあちゃんの家である場合、距離リストから取り出した値が移動距離。
　現在地が友人の家である場合、おばあちゃんの家からの距離を距離リストから取り出し、
　次に訪問する友人の住所までの距離を割り出す。
友人リストが最後まできた場合は、最後にいる場所からおばあちゃんの家までの距離を距離リストから取り出す。

以上を全て加算すれば移動距離が求められる。


-}