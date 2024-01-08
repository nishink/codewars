-- https://www.codewars.com/kata/5a045fee46d843effa000070
-- Factorial decomposition

module FactorialDecomposition.Kata (decomp) where

import Data.List

decomp :: Int -> String
decomp n = let
    showPower xs = show (head xs) ++ if length xs == 1 then "" else "^" ++ show (length xs)
    list = map showPower $ group $ sort $ concat $ map factorization [2..n]
    in intercalate " * " list

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

factorization :: Int -> [Int]
factorization 1 = []
factorization x = v : factorization (x `div` v)
  where
    v = (factors x) !! 1
{-
階乗を素因数分解する。

n = 12; decomp(12) -> "2^10 * 3^5 * 5^2 * 7 * 11"
since 12! is divisible by 2 ten times, by 3 five times, by 5 two times and by 7 and 11 only once.

n = 22; decomp(22) -> "2^19 * 3^9 * 5^4 * 7^3 * 11^2 * 13 * 17 * 19"

n = 25; decomp(25) -> 2^22 * 3^10 * 5^6 * 7^3 * 11^2 * 13 * 17 * 19 * 23

問題文に「階乗はとても大きな数になる。4000!=12674桁の数」と書いているので、
愚直に階乗を計算してから素因数分解したのではタイムアウトになってしまうように思われる。

そこで、2からnまでの数をそれぞれ素因数分解して、
結果を加算していく方法が考えられる。

つまり、
2,3,4,5,6,7,8...
という数列を
2,3,[2,2],5,[2,3],7,[2,2,2]...
と分解してゆき、
最終的に2が何個,3が何個,5が何個...と数えればいい。

-}