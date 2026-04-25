-- https://www.codewars.com/kata/59ccf051dcc4050f7800008f
-- Buddy Pairs

module Buddy.Kata (buddy) where

buddy :: Int -> Int -> Maybe (Int, Int)
buddy start lim = if null f then Nothing else head f
  where
    s n = sum $ divisors n
    f = [ Just (n, m) | n <- [start..lim], let m = s n - 1, m > n, s m == n + 1 ]

-- 約数を求める
divisors :: Int -> [Int]
divisors n = [ x | x <- [1..n-1], n `mod` x == 0 ]

{-
問題文

数の約数が何であるかはご存知でしょう。
正の整数 n の約数は、n 自身以外の約数だけを考慮すると、適切な約数であると言われます。
以下の説明では、約数は適切な約数を意味します。
たとえば、100 の場合、適切な約数は 1、2、4、5、10、20、25、50 です。

s(n)をnのこれらの適切な約数のsumとします。各数の適切な約数のsumが他の数より1多いように、buddy2つの正の整数を呼び出します。

(n, m) are a pair of buddy if s(m) = n + 1 and s(n) = m + 1

例えば、48と75はそのようなペアです：

48の約数は：1、2、3、4、6、8、12、16、24 -->合計：76 = 75 + 1
75の約数は次のとおりです。1、3、5、15、25 -->合計：49 = 48 + 1

タスク

startとlimit2つの正の整数が与えられた場合、関数buddy(start, limit)、
n（正の整数）がstart（inclusive）とlimit（inclusive）の間にあるように、
buddy pairsの最初のペア(n m)を返す必要があります。mはlimitよりも大きく、nより大きくなければなりません。

条件を満たすbuddy pairがない場合は"Nothing"または（Go langの場合）nilまたは（Dartの場合）nullを返します。
（Lua、Pascal、Perl、Dの場合[-1, -1]Erlang { -1、-1 }の場合）。

解き方

startからlimitまでの数に対して、その数の適切な約数のsumを求める。
約数のsumがlimitより大きいものがあれば、それが答えとなる。
複数ある場合は最初に見つかったものが答えとなる。


-}
