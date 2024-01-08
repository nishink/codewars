-- https://www.codewars.com/kata/534ffb35edb1241eda0015fe
-- Blackjack Scorer

module BlackJack where 

import Control.Monad
import Data.List

scoreHand :: [String] -> Int
scoreHand xs = let
  aces = filter (=="A") xs
  others = filter (/="A") xs
  otherScore = sum $ map score others
  acesScore = scoreAces aces
  scores = map (+otherScore) acesScore
  (bj, bust) = break (21 <) scores
  in if bj /= [] then maximum bj else minimum bust

scoreAces :: [String] -> [Int]
scoreAces as = nub $ map sum $ replicateM (length as) [1,11]

score :: String -> Int
score s
  | elem s ["K","Q","J"] = 10
  | otherwise = read s

