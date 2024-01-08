-- https://www.codewars.com/kata/51fc12de24a9d8cb0e000001
-- ISBN-10 Validation

module ISBN10 where

import Data.Char

validISBN10 :: String -> Bool
validISBN10 s = if length s /= 10 then False else check s
    where
        check s = all (flip elem ['0'..'9']) (init s) && (flip elem ('X':['0'..'9'])) (last s) && calc s `mod` 11 == 0
        calc s = sum [ (if c == 'X' then 10 else digitToInt c) * i | (c,i) <- zip s [1..] ]

{-
https://ja.wikipedia.org/wiki/ISBN
ISBN-10とは、2006年までの旧規格。
10桁目がチェックディジットで、格桁の値とインデックスをかけたものの総和を11で割って余りが0になればOK。
Xは10を表す。
-}
