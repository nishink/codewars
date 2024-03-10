-- https://www.codewars.com/kata/5fe26f4fc09ce8002224e95d
-- The Dots and Parentheses

module DotsAndParentheses (puzzle) where

import Data.List

puzzle :: Int -> String
puzzle 0 = "."
puzzle 1 = "()"
puzzle n = "(" ++ encodeNumber n ++ ")"

encodeNumber :: Int -> String
encodeNumber n = let
  exponents = [ (head i, length i) | i <- group $ factors n ]
  in concat $ map puzzle $ encodeExponent exponents primes 1

encodeExponent :: [(Int,Int)] -> [Int] -> Int -> [Int]
encodeExponent [] _ _ = []
encodeExponent exps@((x,e):es) (p:ps) i = 
  if x == p then e : encodeExponent es ps (i+1) else 0 : encodeExponent exps ps (i+1)

-- エラトステネスのふるい
seive (x:xs) = x : seive (filter (\y -> y `mod` x /= 0) xs)
-- 素数
primes = seive [2..]
-- 素因数分解
factor n (x:xs) | n `mod` x == 0 = x | otherwise = factor n xs
factors 1 = []
factors n = let p = factor n primes in p : factors (n `div` p)

{-
次のタスクには2つの部分があります。まず、次の自然数のエンコーディングの背後にある原理を理解する必要があります。
下の表は、0から11までの数字のエンコーディングを示しています。

番号 -------> コード

0 -------> '.'
1 -------> '()'
2 -------> '(())'
3 -------> '(.())'
4 -------> '((()))'
5 -------> '(..())'
6 -------> '(()())'
7 -------> '(...())'
8 -------> '((.()))'
9 -------> '(.(()))'
10 -------> '(().())'
11 -------> '(....())'
数字のエンコード方法を理解したら、与えられた自然数をエンコードするプログラムを書いて文字列として返します。

0から10000までの値がチェックされます

完了後に私のカタを評価してください


ChatGPTに聞いたらencodeNumberという関数を出力してくれたが、期待する動きではなかった。
ただ、どんな関数を書いたらいいかのイメージはわかった。

ググってみると、法則を解説している動画があった。
https://www.youtube.com/shorts/JY0_ApbZYkQ?app=desktop

これによると、素因数分解した結果がカギのようだ。

0 -> "."
1 -> "()"
2 -> 2^1 -> (1) -> "(" + "()" + ")" -> "(())"
3 -> 2^0 * 3^1 -> (01) -> "(" + "." + "()" + ")" -> "(.())"
4 -> 2^2 -> (2) -> "(" + "(())" + ")" -> "((()))"

n -> 素因数分解 -> 各素数の指数のみ取り出す -> 点と括弧に変換 -> 連結したものを括弧で囲む


-}