-- https://www.codewars.com/kata/52bc74d4ac05d0945d00054e
-- First non-repeating character

module NonRepeating (firstNonRepeatingLetter) where

import Data.Char (toLower)
import Data.List (find)

-- | Returns the first unique letter (case-insensitive), if it exists, from the given string.
firstNonRepeatingLetter :: String -> Maybe Char
firstNonRepeatingLetter str = find isUnique str
    where
        lowerStr = map toLower str
        isUnique c = count (toLower c) lowerStr == 1
        count x = length . filter (== x)

{-
first_non_repeating_letter†という名前の関数を作成し、文字列の入力を受け取り、
文字列のどこにも繰り返されない最初の文字を返します。

たとえば、入力'stress'が与えられた場合、関数は't'を返す必要があります。
なぜなら、文字tは文字列で一度だけ発生し、文字列で最初に発生するからです。

追加の課題として、大文字と小文字は同じ文字と見なされますが、
関数は最初の文字に対して正しい大文字を返す必要があります。
たとえば、入力'sTreSS'は'T'を返す必要があります。

文字列にすべての繰り返し文字が含まれている場合、空の文字列を返す必要があります ("");

†注：この関数は歴史的な理由からfirstNonRepeatingLetterと呼ばれていますが、
関数は任意のUnicode文字を処理する必要があります。

---
解き方：
    まずは文字列をすべて小文字に統一する。
    その後、各文字の出現回数をカウントする。
    最後に、元の文字列を順に見ていき、出現回数が1の文字を探す。
    もし見つかれば、その文字を返す。見つからなければNothingを返す。

-}