-- https://www.codewars.com/kata/56b2abae51646a143400001d
-- Mystery Function

module MysteryFunction (mystery,mysteryInv,nameOfMystery) where

import Data.Bits

mystery :: Int -> Int
mystery n = n `xor` (shift n (-1))

mysteryInv :: Int -> Int
mysteryInv = bin2dec . decodeGray . dec2bin

nameOfMystery :: String
nameOfMystery = "Gray code"

dec2bin :: Int -> [Int]
dec2bin n = reverse $ convert n
    where
        convert :: Int -> [Int]
        convert 0 = []
        convert n = n `mod` 2 : convert (n `div` 2)

decodeGray :: [Int] -> [Int]
decodeGray [] = []
decodeGray (n:ns) = scanl xor n ns

bin2dec :: [Int] -> Int
bin2dec ns = sum $ zipWith (*) (iterate (*2) 1) (reverse ns)

{-
隣り合う数字のビットが１しか違わないので、
この関数はグレイコードであるというのは探せばすぐに見つかった。
nameOfMysteryはおそらくgray codeだろう。

二進数をグレイコードに変換するには、
元の値と右に１ビットシフトした値の排他的論理和を取ればいい。

n xor (shift n (-1))

グレイコードを二進数に変換するには、
最上位桁のみそのままに、上位桁から順に排他的論理和をとった値を適用する。

まずは10進数を2進数(数字のリスト)に変換する処理を作り、
上記の変換処理を適用する。
最後に数字のリストをIntに変換すればよい。

-}