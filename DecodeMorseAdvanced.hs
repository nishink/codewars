-- https://www.codewars.com/kata/54b72c16cd7f5154e9000457
-- Decode the Morse code, advanced

module Kata.DecodeMorseAdvanced where

--import Kata.DecodeMorseAdvanced.Preload
    -- holds the "morseCodes" data structure for you

import Data.List
import Data.List.Split (splitOn)
import Data.Map.Strict ((!))

decodeBits :: String -> String
decodeBits bits = let
    t = trim bits '0'
    u = unit t
    decode b 
      | b == replicate (u*7) '0' = "   "
      | b == replicate (u*3) '0' = " "
      | b == replicate u '0' = ""
      | b == replicate (u*3) '1' = "-"
      | b == replicate u '1' = "."
  in concat $ map decode $ group t

decodeMorse :: String -> String
decodeMorse = unwords . filter (not . null) . map (concatMap (morseCodes!) . words) . splitOn "   "

unit :: String -> Int
unit = minimum . map length . group

trim :: String -> Char -> String
trim s c = let f = dropWhile (==c) . reverse in f $ f s


heyjude = "1100110011001100000011000000111111001100111111001111110000000000000011001111110011111100111111000000110011001111110000001111110011001100000011"
