-- https://www.codewars.com/kata/58e77c88fd2d893a77000102
-- City Swim - 2D (TowerFlood And PlainFlood)

module CitySwim (rainVolume) where

import Data.List

rainVolume :: [Int] -> Int
rainVolume xs = let
    ps = 0 : pos (pickPeaks xs) ++ [length xs - 1]
    in countVolume xs ps

countVolume :: [Int] -> [Int] -> Int
countVolume _ [] = 0
countVolume _ [p] = 0
countVolume xs (px:py:pz) = count px py + countVolume xs (py:pz)
    where
        count x y = let
            tall = min (xs !! x) (xs !! y)
            in sum [ tall - (xs !! i) | i <- [x+1..y-1]]

--

data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int]} deriving (Eq, Show)
pickPeaks :: [Int] -> PickedPeaks
pickPeaks ns = pick ns 1 [] []

pick :: [Int] -> Int -> [Int] -> [Int] -> PickedPeaks
pick (a:b:c:ds) idx pos peaks
    | a < b && b > c = pick (c:ds) (idx+2) (pos ++ [idx]) (peaks ++ [b])
    | a < b && b == c = let
        plateau = takeWhile (c==) ds
        remain = dropWhile (c==) ds
        in if null remain then PickedPeaks { pos = pos, peaks = peaks }
            else if c > head remain then pick remain (idx + 3 + length plateau) (pos ++ [idx]) (peaks ++ [b])
            else pick (c:remain) (idx + 2 + length plateau) pos peaks
    | otherwise      = pick (b:c:ds) (idx + 1) pos peaks
pick _ _ pos peaks = PickedPeaks { pos = pos, peaks = peaks }

{-
PickPeaksを応用してできないか試してみたが、
以下のようなケースで対応できないことが判明した。

＼　　　　　　／
　＼　　／＼／
　　＼／

考え方を変えてみる。

一番高いものがまず基準になるので、
一番高いもののindexを取得する。
先頭から一番高いものの間と一番高いものから終端について、
次に高いもののindexを取得する。
一番高いものと次に高いものの間は、次に高いものを基準に水量を計測する。
次に高いものが端なら終了。
次に高いものが端でなければ、残りの部分について再起的に繰り返す。

一番高いものでリストを分割する。
前半と後半でそれぞれ一番高いものを取り出す。
前半の一番高いものから全体の一番高いものの間、
全体の一番高いものから後半の一番高いものの間は、水量が確定する。
前半の一番高いものより前と、後半の一番高いものより後は、
それぞれの一番高いものを基準に再起的に繰り返す。



-}

getPeakIndex xs = head $ elemIndices (maximum xs) xs

breakPeakIndex xs = let
    tall = maximum xs
    idx = head $ elemIndices tall xs
    fstHalf = take idx xs
    sndHalf = drop (idx+1) xs
    in (idx, tall, fstHalf, sndHalf)

countFirstHalf [] _ = 0
countFirstHalf xs top = let
    (idx, tall, fstHalf, sndHalf) = breakPeakIndex xs
    in countFirstHalf fstHalf idx + (tall * length sndHalf) - sum sndHalf

countSecondHalf [] _ = 0
countSecondHalf xs top = let
    (idx, tall, fstHalf, sndHalf) = breakPeakIndex xs
    in countSecondHalf sndHalf idx + (tall * length fstHalf) - sum fstHalf

rainVolume2 [] = 0
rainVolume2 xs = let
    (idx, _, fstHalf, sndHalf) = breakPeakIndex xs
    in countFirstHalf fstHalf idx + countSecondHalf sndHalf idx


