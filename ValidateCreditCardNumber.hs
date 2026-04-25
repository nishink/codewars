-- https://www.codewars.com/kata/5418a1dd6d8216e18a0012b2
-- Validate Credit Card Number

module Validate where

validate :: Integer -> Bool
validate n = (sumDigits . doubleEverySecond . toDigits) n `mod` 10 == 0
  where
    toDigits 0 = []
    toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

    doubleEverySecond xs = reverse $ zipWith ($) (cycle [id, (*2)]) (reverse xs)

    sumDigits = sum . concatMap toDigits

{-
説明：

このカタでは、クレジットカード番号の検証に役立つLuhnアルゴリズムを実装します。

正の整数（最大16桁）を受け取り、それが有効なクレジットカード番号であれば true、
そうでなければ false を返します。

これがアルゴリズムです。

2桁目（右から）から、右から左にスキャンし、他の数字を2倍にします。

別の考え方は、偶数の数字がある場合は、最初の桁から他のすべての桁を2倍にし、
奇数桁の数字がある場合は、2番目の数字から他のすべての桁を2倍にします。

1714 ==> [1*, 7, 1*, 4] ==> [2, 7, 2, 4]

12345 ==> [1, 2*, 3, 4*, 5] ==> [1, 4, 3, 8, 5]

891 ==> [8, 9*, 1] ==> [8, 18, 1]
結果の数値が9より大きい場合は、その数値の合計に置き換えます（9を引くのと同じです）。

[8, 18*, 1] ==> [8, (1+8), 1] ==> [8, 9, 1]

or:

[8, 18*, 1] ==> [8, (18-9), 1] ==> [8, 9, 1]
最後の数字をすべて合計します。

[8, 9, 1] ==> 8 + 9 + 1 = 18
最後に、その合計を10で割ってください。残りがゼロの場合、元のクレジットカード番号は有効です。

18 (modulus) 10 ==> 8 , which is not equal to 0, so this is not a valid credit card number
-}
