-- https://www.codewars.com/kata/558878ab7591c911a4000007
-- Single Word Pig Latin

module Codewars.PigLatin where

import Data.Char
import Data.List

pigLatin :: String -> Maybe String
pigLatin xs = if not (isAlphaStr xs) then Nothing else Just (map toLower $ pig xs)

isAlphaStr :: String -> Bool
isAlphaStr s = and $ map isAlpha s

isVowel :: Char -> Bool
isVowel c = elem (toLower c) ['a','e','i','o','u']

isConsonant :: Char -> Bool
isConsonant c = not (isVowel c)

pig :: String -> String
pig s = if isVowel (head s) then s ++ "way"
        else let
          cons = takeWhile isConsonant s
        in drop (length cons) s ++ cons ++ "ay"
