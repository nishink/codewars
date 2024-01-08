-- https://www.codewars.com/kata/5ad1e412cc2be1dbfb000016
-- Missing Alphabet

module MissingAlphabet where

import Data.Char
import Data.List

insertMissingLetters :: [Char] -> [Char]
insertMissingLetters input = process input ""
    where
        missingLetters = ['a'..'z'] \\ input
        process "" acc = acc
        process (x:xs) acc = let
            ms = if elem x acc then ""
                    else map toUpper $ filter (x<) missingLetters
            in process xs (acc ++ [x] ++ ms)
