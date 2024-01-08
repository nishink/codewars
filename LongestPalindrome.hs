-- https://www.codewars.com/kata/54bb6f887e5a80180900046b
-- longest_palindrome

module Codewars.Kata.LongestPalindrome where

longestPalindrome :: Eq a => [a] -> Int
longestPalindrome [] = 0
longestPalindrome (x:xs) = max (maxPalindrome (x:xs)) (longestPalindrome xs)

isPalindrome xs = xs == reverse xs

maxPalindrome xs
    | isPalindrome xs = length xs
    | otherwise = maxPalindrome $ init xs



{-
最も長い回文を探す。
・先頭からスタートして、逆順にしたら回文になってるかチェック。
・長い方からチェックして、回文だったら終了。その時の長さを記録。
・一文字ずつ次をチェック。


-}