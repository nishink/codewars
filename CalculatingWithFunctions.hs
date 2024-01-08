-- https://www.codewars.com/kata/525f3eda17c7cd9f9e000b39
-- Calculating with Functions

module CalculatingWithFunctions (plus,minus,times,dividedBy,zero,one,two,three,four,five,six,seven,eight,nine) where

plus,minus,times,dividedBy :: ((Int -> Int) -> Int) -> (Int -> Int)
plus x = (+) (x (+0))
minus x = flip (-) (x (+0))
times x = (*) (x (*1))
dividedBy x = flip div (x (*1))

zero,one,two,three,four,five,six,seven,eight,nine :: (Int -> Int) -> Int
zero x = x 0
one x = x 1
two x = x 2
three x = x 3
four x = x 4
five x = x 5
six x = x 6
seven x = x 7
eight x = x 8
nine x = x 9

{-
関数だけで数値の計算をする。
数値 $ 演算子 $ 数値
という形が決まっていて、最終的には数値を返す必要がある。

なので、数値の型は「関数を受け取って数値を返す型」となり、
演算子の型は、「「関数を受け取って数値を返す型」を受け取って関数を返す型」となる。

-}