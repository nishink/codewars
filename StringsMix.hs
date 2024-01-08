-- https://www.codewars.com/kata/5629db57620258aa9d000014
-- Strings Mix

module Codewars.G964.Mixin where

import Data.Char
import Data.List

mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = intercalate "/" $ map (\(c, (a1, a2)) -> prefix a1 a2 : ':' : replicate (max a1 a2) c) $ sortA $ cut2 $ appearance2 s1 s2

appearance :: String -> [(Char, Int)]
appearance = map (\x -> (head x, length x)) . group . sort . filter isLower

appearance2 :: String -> String -> [(Char, (Int, Int))]
appearance2 s1 s2 = let
    a1 = appearance s1
    a2 = appearance s2
    f c a = maybe 0 id $ lookup c a
    in [ (c, (f c a1, f c a2)) | c <- ['a'..'z'] ]

cut2 :: [(Char, (Int, Int))] -> [(Char, (Int, Int))]
cut2 a = filter (\x -> ums x >= 2) a

ums :: (Char, (Int, Int)) -> Int
ums x = uncurry max (snd x)

sortA :: [(Char, (Int, Int))] -> [(Char, (Int, Int))]
sortA = sortBy (\x y -> let 
    x1 = ums x
    y1 = ums y
    f (c, (a1, a2)) = (prefix a1 a2, c)
    in if x1 == y1 then compare (f x) (f y) else compare y1 x1)

prefix :: Int -> Int -> Char
prefix a1 a2
    | a1 > a2 = '1'
    | a1 < a2 = '2'
    | otherwise = '='


s1="Are the kids at home? aaaaa fffff"
s2="Yes they are here! aaaaa fffff"
{-
２つの文字列に出現する小文字について、
出現数の多い順に出力する。
その際、文字列１が多いのか、２が多いのか、等しいのかを表す。

s1 = "my&friend&Paul has heavy hats! &"
s2 = "my friend John has many many friends &"
mix(s1, s2) --> "2:nnnnn/1:aaaa/1:hhh/2:mmm/2:yyy/2:dd/2:ff/2:ii/2:rr/=:ee/=:ss"

s1 = "mmmmm m nnnnn y&friend&Paul has heavy hats! &"
s2 = "my frie n d Joh n has ma n y ma n y frie n ds n&"
mix(s1, s2) --> "1:mmmmmm/=:nnnnnn/1:aaaa/1:hhh/2:yyy/2:dd/2:ff/2:ii/2:rr/=:ee/=:ss"

s1="Are the kids at home? aaaaa fffff"
s2="Yes they are here! aaaaa fffff"
mix(s1, s2) --> "=:aaaaaa/2:eeeee/=:fffff/1:tt/2:rr/=:hh"

まずは２つの文字列について、それぞれ小文字以外を除外してソート＆グルーピングし、文字と出現数を割り出す。
a~zの文字について、文字列１と２の出現数をまとめたリストを作る。
出現数の多いもの順に並べる。
出現数が同じものについては、１が多い->２が多い->同数->アルファベットの順に並べる。


-}
