-- https://www.codewars.com/kata/573182c405d14db0da00064e
-- Consecutive k-Primes

module Codewars.G964.Primeconsec where

import Data.List (group)

consecKprimes :: Int -> [Int] -> Int
consecKprimes k xs = sum $ map f $ group $ map (length . factorization) xs
    where
        f ns = if head ns /= k then 0 else length ns - 1

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

factorization :: Int -> [Int]
factorization 1 = []
factorization x = v : factorization (x `div` v)
  where
    v = (factors x) !! 1

{-
-- エラトステネスのふるい
seive (x:xs) = x : seive (filter (\y -> y `mod` x /= 0) xs)
-- 素数
primes = seive [2..]
-- 素因数分解
factor n (x:xs) | n `mod` x == 0 = x | otherwise = factor n xs
-- とても遅い！
factors 1 = []
factors n = let p = factor n primes in p : factors (n `div` p)
-}

-- test
arr :: [Int]
arr = [393989,393958,394039,394059,393994,393961,394078,394063,394047,393964,393998,394053,394034,394043,393962,394054,394076,393979,394017,394049,394062,394029,394007,394022,394005,394042,394073,394028,394031,393967,393977,393997,393987,393995,394055,394019,394037,393993,394013,394003,394001,394079,393970,393969,393999,394025,394077,394051]


{-
K-Primeというのは、素因数分解するとK個の素因数になるもの。

Examples:
k = 2 -> 4, 6, 9, 10, 14, 15, 21, 22, …
k = 3 -> 8, 12, 18, 20, 27, 28, 30, …
k = 5 -> 32, 48, 72, 80, 108, 112, …

タスク：
整数kと正の整数のリストarr与えられた場合、
関数consec_kprimes (or its variants in other languages)は、
シーケンスarr番号で正確にk素因数で2回連続して出てくる回数を返します。

要するに、arrの各要素を素因数分解してKの数を出して、
指定したKと一致するものが連続する箇所が何個あるかを導けば良い。

まずはKを求めるにあたって、素因数分解できるプログラムを探す。

https://userweb.mnet.ne.jp/tnomura/examples/primes.html
最初はここのを使ったが、素因数分解がとても遅くタイムアウトしてしまった。

https://gumfum.hatenablog.com/entry/2018/01/05/014707
次にここのを使った。記事には遅いと書いてあるが、タイムアウトせずに問題が解けたのでよしとする。

-}

