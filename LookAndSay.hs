-- https://www.codewars.com/kata/530045e3c7c0f4d3420001af/haskell
-- Conway's Look and Say - Generalized

module LookAndSay where

import Data.List

lookSay :: Integer -> Integer
lookSay n = read $ concat [ (show $ length elm) ++ [head elm] | elm <- group $ show n ]

{-
以前解いた「Simple Fun #299: Look And Say And Sum」の応用で解ける。
https://www.codewars.com/kata/5922828c80a27c049c000078
-}