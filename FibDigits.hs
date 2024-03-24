-- https://www.codewars.com/kata/5779f894ec8832493f00002d
-- Calculate Fibonacci return count of digit occurrences

module Kata (fibDigits) where

import Data.Ratio
import Data.List

fibDigits :: Integer -> [(Integer, Integer)]
fibDigits = reverse . sort . map (\x -> (fromIntegral (length x), read [head x])) . group . sort . show . fibI . fromIntegral

{-
https://qiita.com/mod_poppo/items/4f78d135bb43b7fd1743
-}
data Ext_phi a = Ext_phi !a !a deriving (Eq,Show)

instance Num a => Num (Ext_phi a) where
  Ext_phi a b + Ext_phi a' b' = Ext_phi (a + a') (b + b')
  Ext_phi a b - Ext_phi a' b' = Ext_phi (a - a') (b - b')
  negate (Ext_phi a b) = Ext_phi (negate a) (negate b)
  Ext_phi a b * Ext_phi a' b' = let bb' = b * b'
                                in Ext_phi (a * a' + bb') (a * b' + b * a' + bb')
  fromInteger n = Ext_phi (fromInteger n) 0

instance Fractional a => Fractional (Ext_phi a) where
  recip (Ext_phi a b) = let s = a * a + a * b - b * b
                        in Ext_phi ((a + b) / s) (- b / s)
  fromRational x = Ext_phi (fromRational x) 0

phi, psi :: (Num a) => Ext_phi a
phi = Ext_phi 0 1
psi = 1 - phi

fib :: Int -> Integer
fib i = case (phi^i - psi^i) / (phi - psi) of
          Ext_phi x 0 | denominator x == 1 -> numerator x
          x -> error $ "calculation error: fib " ++ show i ++ " = " ++ show x

fibI :: Int -> Integer
fibI i = case phi^i - psi^i of
          Ext_phi mx y | 2 * (- mx) == y -> - mx
          x -> error $ "calculation error: fib " ++ show i ++ " * sqrt 5 = " ++ show x

{-
フィボナッチ数列のn番目の数を求め、
その結果に含まれる数字の数を(数,数字)の組にして降順で出力する。

フィボナッチ数列はできるだけ効率的に求める必要があるので、
n番目の数をピンポイントで求めるプログラムを流用した。
-}