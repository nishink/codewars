-- https://www.codewars.com/kata/58235a167a8cb37e1a0000db
-- Pair of gloves

module PairOfGloves (numberOfPairs) where

 -- import Preloaded (Colour(..))
data Colour = Red | Blue | Green deriving (Show,Eq,Ord,Enum,Bounded)

import Data.List (group, sort)

numberOfPairs :: [Colour] -> Int
numberOfPairs = sum . map (`div` 2) . map length . group . sort
