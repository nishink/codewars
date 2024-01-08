-- https://www.codewars.com/kata/57121e6fcdbf63eb94000e51
-- Fastest Code : Count animals

module Kata.CountAnimals (sc) where

--import Preloaded.CountAnimals (names)
names = ["dog","cat","bat","cock","cow","pig","fox","ant","bird","lion","wolf","deer","bear","frog","hen","mole","duck","goat"]

import Data.List
sc :: String -> Int
sc = f names
    where f [] _ = 0
          f (x:xs) str = maximum [n + f xs (iterate (\\ x) str !! n)| n <- [0..(g x str)]]
          g x = length . takeWhile ((== "") . (x \\)) . iterate (\\ x)

{-
先に「Coding 3min : Count animals」を解いていたので、その中でベストプラクティスのものをそのまま適用。
https://www.codewars.com/kata/571249a0159cde165f00088a/solutions/haskell
-}

