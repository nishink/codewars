-- https://www.codewars.com/kata/561e9c843a2ef5a40c0000a4
-- Gap in Primes

module Codewars.G964.GapInPrimes where

gap :: Integer -> Integer -> Integer -> Maybe (Integer, Integer)
gap g m n = fnc primes
    where
        fnc ps = let
            ((f,s),rs) = takeFirstPrimePairs m ps
            in if s<=n then if s-f==g then Just (f,s) else fnc (s:rs) else Nothing

takeFirstPrimePairs :: Integer -> [Integer] -> ((Integer, Integer), [Integer])
takeFirstPrimePairs m ps = let
    (f:s:rs) = dropWhile (<m) ps
    in ((f,s),rs)

-- https://qiita.com/ttatsf/items/510ec0cd4ad99fef9424
primes :: Integral a => [a]
primes = map fromIntegral $ [2, 3] ++ primes'
    where
        primes' = [5] ++ f 1 7 primes'
        f m s (p : ps) = [n | n <- ns, gcd m n == 1] ++ f (m * p) (p * p) ps
            where ns = [x + y | x <- [s, s + 6 .. p * p - 2], y <- [0, 4]]


{-
素数と次の素数の差を探す。
mからnまでの数の間にある素数のうち、
差がgである素数のペアを探し、最初に見つかったものを返す。
見つからなかった場合はNothingを返す。

解法としては以下が考えられる。
・無限素数列を用意する。
・m以上の最初の素数を取り出す。最初の素数がnを超えるならNothing。
・次の素数を取り出す。次の素数がnを超えるならNothing。
・差がgであるなら、取り出した２つの素数を返す。
・差がgでないなら、次の素数を取り出そうとするが、
　次の素数がnを超えるならNothingを返す。
　次の素数がn以下なら、次の素数を取り出して差をチェックする。

-}