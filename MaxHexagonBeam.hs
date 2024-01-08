-- https://www.codewars.com/kata/5ecc1d68c6029000017d8aaf
-- Hexagon Beam Max Sum

module Kata (maxHexagonBeam) where

import Data.List

maxHexagonBeam :: Int -> [Int] -> Int
maxHexagonBeam n lst = let
    hb = hyphenBeams (lengthes n) (cycle lst)
    sb = slashBeams n hb
    bb = backslashBeams n hb
  in maximum [ maximum (map sum b) | b <- [hb, sb, bb] ]

lengthes :: Int -> [Int]
lengthes n = [n..2*n-1] ++ [2*(n-1),2*(n-1)-1..n]

hyphenBeams :: [Int] -> [Int] -> [[Int]]
hyphenBeams [] _ = []
hyphenBeams (l:ls) cyclst = take l cyclst : hyphenBeams ls (drop l cyclst)

slashBeams :: Int -> [[Int]] -> [[Int]]
slashBeams n hb = transpose [ if i <= n then b ++ zeros n b else zeros n b ++ b | (b, i) <- (zip hb [1..]) ]

backslashBeams :: Int -> [[Int]] -> [[Int]]
backslashBeams n hb = transpose [ if i <= n then zeros n b ++ b else b ++ zeros n b | (b, i) <- (zip hb [1..]) ]

zeros :: Int -> [Int] -> [Int]
zeros n b = replicate (2 * n - 1 - length b) 0


{-

 0 1
2 3 4
 5 6

0 1 x
2 3 4
x 5 6

x 0 1
2 3 4
5 6 x

  0 1 2
 3 4 5 6
7 8 9 A B
 C D E F
  G H I

0 1 2 x x
3 4 5 6 x
7 8 9 A B
x C D E F
x x G H I

x x 0 1 2
x 3 4 5 6
7 8 9 A B
C D E F x 
G H I x x

x=0

まずは横一列をそれぞれリストに分割する方法を考える。
n=2のときは2,3,2
n=3のときは3,4,5,4,3
 :
n=nののときはn,n+1,.,n+(n-1),..,n

次に、斜め方向のリストを取り出す方法を考える。
上から左下方向への場合、
横一列リストの先頭から順に１つずつ取っていく。
上から右下方向への場合、
横一列リストの末尾から順に１つずつ取っていく。
しかし、そうすると長さが違うので取れない列が出てくる。

そこで、長さの足りない分については、
上から左下方向への場合、
上半分は末尾に0を、下半分は先頭に0を詰める。
上から左下方向への場合はその逆をする。
そうすると、縦一列に値を見ていけば良いだけになる。

さらにtransposeを使えば、行列の縦横を逆転できるので、
横一列リストと同じ方法で最大値を求めることができる。

-}