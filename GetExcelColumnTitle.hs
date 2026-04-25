-- https://www.codewars.com/kata/56d082c24f60457198000e77
-- Get the Excel column title!

module Codewars.Kata.GetExcelColumnTitle where

import Data.Char (ord, chr)

getColumnTitle :: Integer -> Maybe String
getColumnTitle n 
    | n <= 0 = Nothing
    | otherwise = Just $ go n
  where
    go 0 = ""
    go m = go q ++ [chr (fromIntegral r + ord 'A')]
      where (q, r) = divMod (m - 1) 26
