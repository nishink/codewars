-- https://www.codewars.com/kata/59aa6567485a4d03ff0000ca
-- Prime reduction

module PrimeReduction where 

import Data.Char

solve :: Int -> Int -> Int
solve a b = length [ n | n <- takeWhile (<b) (dropWhile (<a) primes), isFinallyOne n ]

isFinallyOne :: Int -> Bool
isFinallyOne n = sumDigit n []
  where
    sumDigit n ns
      | n == 1    = True
      | elem n ns = False
      | otherwise = let
          next = sum [ (digitToInt d) ^ 2 | d <- show n ]
        in sumDigit next (n:ns)

primes = 2:f [3] [3,5..]
  where
    f (x:xs) ys = let 
        (ps, qs) = span (< x^2) ys
      in  ps ++ f (xs ++ ps) [z | z <- qs, mod z x /= 0]

{-

2->2^2=4->4^2=16->1^2+6^2=37->3^2+7^2=58->5^2+8^2=89->8^2+9^2=145
 ->1^2+4^2+5^2=1+16+25=42
 ->4^2+2^2=20
 最初に戻る

3->3^2=9->9^2=81->8^2+1^2=65->6^2+5^2=61->6^2+1^2=37
 ループ確定

5->5^2=25->2^2+5^2=29->2^2+9^2=85->8^2+5^2=89
 ループ確定

11->1^1+1^1=2
 ループ確定

13->1^2+3^2=10->1^2+0^2=1

17->1^2+7^2=50
 ループ確定

19->1^2+9^2=82->8^2+2^2=68->6^2+8^2=100->1

29->2^2+9^2=85
ループ確定

31->3^2+1^2=10->1

37
ループ確定

問題の条件が50000を上限としているので、各桁の自乗の合計はせいぜい３桁。
4^2+9^2+9^2+9^2+9^2=340
よって、340までの数で１に集約されるものをあらかじめ計算しておく。
その上で、指定された範囲の素数の中で各桁の自乗の合計を計算し、
１に集約される数になるかどうかを判定する。

-}
