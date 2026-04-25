-- https://www.codewars.com/kata/58b8d22560873d9068000085
-- Simple Fun #178: Faulty Odometer

module Odometer (faultyOdometer) where

import Data.Char (digitToInt)

faultyOdometer :: Int -> Int
faultyOdometer n = sum [ 9^index * digitToInt (if digit > '4' then pred digit else digit) | (digit, index) <- zip (reverse $ show n) [0..] ]




{-
ある車の走行距離計は、数字の表示が壊れている。
表示される数字は0から9までの数字で、
4の数字が表示されず、3から5にスキップしてしまう。

走行距離計の表示をもとに、実際の走行距離を求める関数を作成せよ。

例：
n = 13の場合、4をスキップしているので実際の距離は12となる。
n = 15の場合、4と14をスキップしているので実際の距離は13となる。
n = 2003の場合、1461となるはずである。

解き方：
これは0から9のうち、4を除いた９進数であると考える。
4より大きい数字は、数字を１つ減らした上で、
９進数を１０進数に変換する関数を作成する。

Haskellでの９進数の変換は、以下のようになる。
１、９進数を文字列に変換する。
２、文字列の各文字を数値に変換する。
３、各桁の数値に、9の桁数乗の数を掛けて、合計する。
-}
