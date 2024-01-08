-- https://www.codewars.com/kata/546f922b54af40e1e90001da
-- Replace With Alphabet Position

module ReplaceWithAlphabetPosition (alphabetPosition) where

import Data.Char

alphabetPosition :: String -> String
alphabetPosition = unwords . map show . map (\x -> ord x - ord 'a' + 1) . filter (\x -> elem x ['a'..'z']) . map toLower

{-
与えられた文字列に含まれる、アルファベットをその位置に変換する。
aなら1、bなら2に。
大文字は小文字に置き換える。-> map toLower
アルファベット以外は無視。 -> filterで除外
変換後の数字はスペースで結合する。 -> unwords
-}