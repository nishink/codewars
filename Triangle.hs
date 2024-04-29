-- https://www.codewars.com/kata/58281843cea5349c9f000110
-- Complete the triangle pattern

module Triangle (triangle) where

import Data.List

triangle :: Int -> Int -> Maybe String
triangle i j = let
    x = j - i + 1
    -- 指定した数が1からnまでの和であることを判定する
    n = floor $ (sqrt (1 + 8 * fromIntegral x) - 1) / 2
    sumN = n * (n + 1) `div` 2
    adjust = (x `mod` 10 + i) `mod` 10
    in if sumN == x then Just (triToStr $ convert (tri n) adjust) else Nothing

triToStr :: [[Int]] -> String
triToStr tri = intercalate "\n" [ (replicate n ' ') ++ unwords (map show line) | (line,idx) <- zip tri [0..], let n = length tri - idx - 1 ]

-- 数字の始まりをiにするので、数字を逆転させたのち、いくつかずらす
convert :: [[Int]] -> Int -> [[Int]]
convert tri i = [ map (slide i . change) line | line <- tri ]

-- 1234567890 -> 0987654321
change n = [0,9,8,7,6,5,4,3,2,1] !! (n `mod` 10)

slide i n = (n + i) `mod` 10

-- 内側から１始まりで三角形を作る
tri :: Int -> [[Int]]
tri 0 = []
tri 1 = [[1]]
tri 2 = [[3],[1,2]]
tri n = let
    base = tri (n `mod` 3)
    counter = 1 + (maximum $ 0 : concat base)
    in wrapArround n base counter

wrapArround :: Int -> [[Int]] -> Int -> [[Int]]
wrapArround n tri counter
    | length tri == n = tri
    | otherwise = let
        tri2 = addHead tri counter
        counter2 = counter + length tri2
        tri3 = addBottom tri2 counter2
        counter3 = counter2 + length tri3
        tri4 = addLast tri3 counter3
        counter4 = counter3 + length tri4
        in wrapArround n tri4 counter4

addHead :: [[Int]] -> Int -> [[Int]]
addHead tri counter = let
    counts = makeCounter (length tri + 1) counter
    in [ c:t | (c,t) <- zip counts ([]:tri) ]

addBottom :: [[Int]] -> Int -> [[Int]]
addBottom tri counter = let
    counts = makeCounter (length tri + 1) counter
    in tri ++ [counts]

addLast :: [[Int]] -> Int -> [[Int]]
addLast tri counter = let
    counts = makeCounter (length tri + 1) counter
    in [ t ++ [c] | (c,t) <- zip (reverse counts) ([]:tri) ]

makeCounter :: Int -> Int -> [Int]
makeCounter n counter = map (`mod` 10) $ take n [counter..]



{-
m n を指定して三角形を作る。
三角形はてっぺんから始まり時計回り。
２桁以上を指定した場合、使う数字は下一桁のみ。
三角形が作れない場合は空文字列を返す。

ChatGPTに作らせてみたがコンパイルが通らない。
通すように直させても、ちゃんとしたプログラムになってくれない。

いくつかのパターンを考えてみよう。

triangle 1 1 =
    1

triangle 1 3 =
     1
    3 2

triangle 1 6 =
      1
     6 2
    5 4 3

triangle 1 10 =
       1
      9 2
     8 0 3
    7 6 5 4

triangle 1 15 =
        1
       2 2
      1 3 3
     0 5 4 4
    9 8 7 6 5

４つめのパターンは１つめのパターンを内包しているので、
繰り返しで表現することができそう。

1+2+3+...+n=n(n+1)/2
x=n(n+1)/2
2x=n(n+1)
n^2+n-2x=0

ax^2+bx+c=0の解は
x = -b±√(b^2-4ac)/2a

a=1,b=1,c=-2xなので
n=-1±√(1+8x)/2
±がマイナスだとnが負数になってしまうので、
n=-1+√(1+8x)/2のケースだけ考えれば良い。

nが求まったら、n mod 3によってベースが決まる。
n mod 3 == 1のとき、ベースはtriangle 1 1
n mod 3 == 2のとき、ベースはtriangle 1 3
n mod 3 == 0のとき、ベースはtriangle 1 6
内側から逆順に三角形を作っていく。

ベースの三角形の前に二行、後ろに一行追加し、
ベースの一行前から先頭に番号をふっていく。
ベースの後ろ一行まで到達したら、後ろ一行はn個の値を並べる。
今度はベースの一番下の行からベースの二行前に向かって末尾に数字を足す。

 3
1 2

        5
4↓      4
5   3   3
6  1 2  2
7→8 9 0 1↑

最終的に数字を反転させれば時計回りで内巻きになる。



-}