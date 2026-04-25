-- https://www.codewars.com/kata/55b7bb74a0256d4467000070
-- Number of Proper Fractions with Denominator d

module ProperFractions.JorgeVS.Kata where

-- オイラーのトーシェント関数を使って、d と互いに素な数の数を計算する
totient :: Integer -> Integer
totient 1 = 0
totient d = d * product [(p - 1) | p <- uniquePrimeFactors d] `div` product [p | p <- uniquePrimeFactors d]

-- d の重複なしの素因数を求める関数
uniquePrimeFactors :: Integer -> [Integer]
uniquePrimeFactors n = unique (primeFactors n)
  where
    unique [] = []
    unique (x:xs) = x : unique (filter (/= x) xs)

-- d の素因数を求める関数
primeFactors :: Integer -> [Integer]
primeFactors n = factor n 2
  where
    factor n d
      | d * d > n        = [n | n > 1]  -- d^2 > n の場合、n 自身が素数
      | n `mod` d == 0   = d : factor (n `div` d) d
      | otherwise        = factor n (d + 1)

-- d を分母に持つ真分数の数を計算する
properFractions :: Integer -> Integer
properFractions d = totient d
{-
import Data.List

properFractions :: Integer -> Integer
properFractions n = let
    ps = nub $ primeFactors n
    in fromIntegral $ length [ x | x <- [1..n-1], notElem 0 (map (mod x) ps) ]

-- 素因数分解
primeFactors :: Integer -> [Integer]
primeFactors n = factor n 2 ++ oddFactors n 3
  where
    -- 2で割り切れる限り素因数2を求める
    factor n d
      | n `mod` d == 0 = d : factor (n `div` d) d
      | otherwise = []

    -- 3以上の奇数で割り切れる限り素因数を求める
    oddFactors 1 _ = []
    oddFactors n d
      | d * d > n = [n] -- 割る数が√nを超えたら、それ自体が素数である
      | n `mod` d == 0 = d : oddFactors (n `div` d) d
      | otherwise = oddFactors n (d + 2) -- 次の奇数を試す
-}
{-
分母dの適切な分数の数

nが分子であり、dが分数の分母である場合、その分数は、GCD(n, d) = 1の場合に限り、
（還元された）適切な分数として定義されます。

例えば5/16は適切な分数ですが、6/16はそうではありません。
6と16はどちらも2で割り切れるため、分数を3/8に減らすことができます。

さて、与えられた数dを考慮すると、dを分母として使用して、いくつの適切な分数を構築できますか？

たとえば、dが15であると仮定します。
1/15、2/15、4/15、7/15、8/15、11/15、13/15、14/15の0と1の間の合計8つの異なる適切な分数を作成できます。

与えられた分母で構築できる適切な分数の数を計算する関数を書くことになります。

--
直感的には、分母を素因数分解してできる数字を篩に掛ければ良いように思える。

分母が15なら、素因数分解すると[3,5]なので、
1~14から3,5の倍数を篩に掛ければよい。

素数を求めるエラトステネスの篩から、素数ではなく素因数を指定するやり方にしたらどうか。
と思って書いてみたが、タイムアウトする。

ChatGPTに聞いてみたところ、オイラーのトーシェント関数を使えば良いとのこと。
重複する素因数を省いて、dと互いに素な数を数える。

-}