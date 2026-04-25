-- https://www.codewars.com/kata/559536379512a64472000053
-- Playing with passphrases

module Codewars.Kata.PlayPass (playPass) where

import Data.Char (toUpper, toLower)

playPass :: String  -> Int -> String
playPass s shift = reverse $ zipWith transform s [0..]
  where
    transform c index = updownCase (nineComplement (cyclicShift c shift)) index

-- アルファベットの場合に適用する円シフト
cyclicShift :: Char -> Int -> Char
cyclicShift c shift
  | 'A' <= c && c <= 'Z' = toEnum $ baseA + (fromEnum c - baseA + shift) `mod` 26
  | 'a' <= c && c <= 'z' = toEnum $ basea + (fromEnum c - basea + shift) `mod` 26
  | otherwise             = c
  where
    baseA = fromEnum 'A'
    basea = fromEnum 'a'    

-- 数字に適用する９の補数
nineComplement :: Char -> Char
nineComplement c
  | '0' <= c && c <= '9' = toEnum $ fromEnum '9' - (fromEnum c - fromEnum '0')
  | otherwise             = c

-- 奇数位置を小文字、偶数位置を大文字に変換
updownCase :: Char -> Int -> Char
updownCase c index
  | even index = toEnum $ fromEnum (toUpper c)
  | odd index  = toEnum $ fromEnum (toLower c)

{-
説明：

誰もがパスフレーズを知っています。詩、歌、映画の名前などからパスフレーズを選択できますが、
一般的な文化的参照により推測されることがよくあります。
さまざまな方法でパスフレーズを強化することができます。1つは以下の通りです。

数字とアルファベット以外の文字を含む大文字のテキストを選択します。

1.各文字を与えられた数ずつシフトしますが、変換された文字は文字でなければなりません（円シフト）、
2.各数字を9の補数に置き換えます。
3.アルファベット以外の文字や数字以外の文字はそのままにしておきます。
4.奇数位置の各文字をダウンケース、偶数位置の各文字をアップケース（最初の文字は0の位置）、
5.全体の結果を逆転させる。

例：

あなたのテキスト：「BORN IN 2015!」、シフト1

1 + 2 + 3 -> 「CPSO JO 7984！」

4「CpSo jO 7984！」

5 "！4897 Oj oSpC」

パスフレーズが長いので、小さくて簡単なプログラムがある方が良いです。書いてくれませんか？


-}