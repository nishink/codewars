-- https://www.codewars.com/kata/5e18743cd3346f003228b604
-- How many nines?

module HowManyNines where

nines :: Integer -> Integer
nines n = countNines n (countDigit n)

-- nの桁数を求める
countDigit :: Integer -> Integer
countDigit n = fromIntegral $ length $ show n

-- nの桁数とnを受け取り、nの中に含まれる9の数を求める
countNines :: Integer -> Integer -> Integer
countNines 0 _ = 0
countNines n m
  | n < 10 = if n >= 9 then 1 else 0
  | otherwise = let
      highestDigit = n `div` (10 ^ (m - 1))
      remainingDigits = n `mod` (10 ^ (m - 1))
    in if highestDigit == 9
       then highestDigit * countNinesInDigits (m - 1) + remainingDigits + 1
       else highestDigit * countNinesInDigits (m - 1) + countNines remainingDigits (m - 1)

-- m桁の数の中に9が含まれる数を求める
countNinesInDigits :: Integer -> Integer
countNinesInDigits 0 = 0
countNinesInDigits 1 = 1
countNinesInDigits m = 9 * countNinesInDigits (m - 1) + 10 ^ (m - 1)

-- 別解（タイムアウトする）
nines' n = sum [1 | x <- [0..n], '9' `elem` show x]

{-
0からnまでの整数の中に9を含む数がいくつあるかを求める。

9を含む数を数えるために、0からnまでの整数を順に取り出し、
文字列に変換して、その中に'9'が含まれているかを調べる。
というやり方では、大きい数に対しては効率が悪くタイムアウトしてしまう。
nines n = sum [1 | x <- [0..n], '9' `elem` show x]

考え方を変えてみよう。
例えば、1の位に9を含む数は1つ。

10の位が0~8の場合は、10の位の数*1と、1の位が9なら+1した数が、9を含む数になる。
10の位が9の場合は、上記の0~8までの9を含む数に加えて、1の位の数+1が9を含む数になる。
これにより、n=99の場合は、0を含む数は19個になる。
(8*1 + 1 + 9 + 1 = 19)

100の位についても同様に考える。
100の位が0~8の場合は、100の位の数*19と、10以下の値に9を含む数を足したものが、9を含む数になる。
100の位が9の場合は、上記の0~8までの9を含む数 + 10以下の位の数+1が9を含む数になる。

これにより、n=999の場合は、0を含む数は271個になる。
((8+1)*19 + 99 + 1 = 271)

これを一般化して、
m桁の数nについて、9が含まれる数の個数は、
mの位が0~8の場合は、m桁目の数*countNinesInDigits (m-1) + nines (nからm桁目を落としたもの) が9を含む数になる。
mの位が9の場合は、m桁目の数*countNinesInDigits (m-1) + (nからm桁目を落としたもの) + 1 が9を含む数になる。
-}