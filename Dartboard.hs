-- https://www.codewars.com/kata/5870db16056584eab0000006
-- Let's Play Darts!

module Codewars.Dartboard.Detection where

-- import Codewars.Dartboard.Types
data DartScore
         = MissDS
         | SingleDS Int
         | DoubleDS Int
         | TripleDS Int
         | BullDS
         | DoubleBullDS
         deriving (Eq, Show)

-- | Detect a dart score with the coordinates of the hit.
getDartScore :: Double -> Double -> DartScore
getDartScore x y = let
    d = distance x y
    a = angle2score $ angle x y
    in 
        if d <= 12.7/2 then DoubleBullDS
        else if d <= 31.8/2 then BullDS
        else if d <= 198/2 then SingleDS a
        else if d <= 214/2 then TripleDS a
        else if d <= 324/2 then SingleDS a
        else if d <= 340/2 then DoubleDS a
        else MissDS

angle :: Double -> Double -> Int
angle x y = let
    r = atan2 y x
    rr = if r < 0 then r + 2 * pi else r
    in floor (rr * 360 / (2 * pi))

angle2score :: Int -> Int
angle2score a = let
    idx = (a + 9) `div` 18
    in [6,13,4,18,1,20,5,12,9,14,11,8,16,7,19,3,17,2,15,10,6] !! idx

distance :: Double -> Double -> Double
distance x y = sqrt (x^2 + y^2)

{-
ダーツの矢が刺さった座標から得点を計算する。
的の中心を原点(0,0)とする。
中心からの距離と角度を求めれば得点が決まる。
-}