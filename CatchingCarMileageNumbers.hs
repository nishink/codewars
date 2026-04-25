-- https://www.codewars.com/kata/52c4dd683bfd3b434c000292
-- Catching Car Mileage Numbers

module Awesome.Numbers where

import Data.List

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs = let
    criteria = [isZeroSeq x, isSameNum x, isIncrement x, isDecrement x, isPalindrome x, isAwesome x xs]
    in if elem Yes criteria then Yes else if elem Almost criteria then Almost else No

check :: (Integer -> Bool) -> Integer -> Answer
check f x = if f x then Yes else if or (map f [x-2, x-1, x+1, x+2]) then Almost else No

isZeroSeq :: Integer -> Answer
isZeroSeq x = let
    f n = n > 99 && all (=='0') (tail $ show n)
    in check f x

isSameNum :: Integer -> Answer
isSameNum x = let
    f n = n > 99 && all (==y) ys where (y:ys) = show n
    in check f x

isIncrement :: Integer -> Answer
isIncrement x = let
    f n = n > 99 && isInfixOf (show n) "1234567890"
    in check f x

isDecrement :: Integer -> Answer
isDecrement x = let
    f n = n > 99 && isInfixOf (show n) "9876543210"
    in check f x

isPalindrome :: Integer -> Answer
isPalindrome x = let
    f n = n > 99 && show n == reverse (show n)
    in check f x

isAwesome :: Integer -> [Integer] -> Answer
isAwesome x xs = let
    f n = n > 99 && elem n xs
    in check f x

{-
キリ番ゲットのための関数。
キリのいい番号の場合はYes、前後２以内のときはAlmost、それ以外はNoを返す。
キリ番の条件は以下の通り。

・すべてのゼロが続く任意の数字：100、90000
・すべての数字は同じ数字です：1111
・数字は順次であり、†：1234
・数字は順次、減少しています‡:4321
・数字は回文です1221または73837
・数字は、awesomePhrases配列の値の1つと一致します。

・数字は99より大きい場合にのみ興味深いです！
・入力は常に0より大きく、1,000,000,000未満の整数になります。
・awesomePhrases配列は常に提供され、常に配列になりますが、空である可能性があります。
　（誰もが数字が面白い単語を綴ると思っているわけではありません...）
・0、１、または2のみ出力する必要があります。
　注：Haskellでは、0,1,2の代わりにNo、Almost、Yesを使用します。

-}