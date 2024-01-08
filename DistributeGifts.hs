-- https://www.codewars.com/kata/584f7b27d8912ab46e0000d5
-- Christmas mission: Distribute gifts #5

module DistributeGifts.Kata (distributeGifts) where

import Data.List
import Data.Function (on)

distributeGifts :: String -> Either String Int
distributeGifts maps = let
    pos = sort $ mapToPos maps
    santa = filter (\(m, _) -> m == 's') pos
    children = filter (\(m, _) -> m /= 's') pos
  in if santa == [] then Left "Where is Santa Claus?"
     else Right (countStep (santa ++ children))

mapToPos :: String -> [(Char, (Int, Int))]
mapToPos maps = m2p maps 0 0 []
  where
    m2p [] _ _ ps = ps
    m2p (m:ms) x y ps = case m of
      '\n' -> m2p ms 0 (y+1) ps
      '.'  -> m2p ms (x+1) y ps
      _    -> m2p ms (x+1) y (ps ++ [(m, (x, y))])

countStep :: [(Char, (Int, Int))] -> Int
countStep [] = 0
countStep [p] = 0
countStep (p:q:ps) = let
    (x0, y0) = snd p
    (x1, y1) = snd q
  in abs (x1-x0) + abs (y1-y0) + countStep (q:ps)


comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs

comb' :: Int -> [(Int,Int)] -> Int -> [[(Int,Int)]]
comb' 0 xs _ = [[]]
comb' _ [] _ = []
comb' n (x:xs) m = [ x:y | y <- comb' (n-1) xs m, (sum . map fst) (x:y) <= m ] ++ comb' n xs m


f :: Int -> [Int] -> [Int]
f 0 _ = []
f n (x:xs)
  | n < 0 = []
  | n < x = f n xs
  | otherwise = x : f (n-x) xs

knapsackDP :: Int -> [(Int,Int)] -> [(Int,Int)]
knapsackDP maxCapacity = snd . last . foldl step initial
  where initial = replicate (maxCapacity + 1) empty
        empty = (0, [])  -- (value, items)
        step previous item = zipWith chooseValuable itemIncluded notIncluded
          where itemIncluded = shift ++ map addItem previous
                notIncluded = previous
                addItem (v, items) = (v +  snd item, item : items)
                chooseValuable a b = maximumBy (compare `on` fst) [a, b]
                shift = replicate (fst item) empty
