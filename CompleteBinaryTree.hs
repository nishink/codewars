-- https://www.codewars.com/kata/5c80b55e95eba7650dc671ea
-- Complete Binary Tree

module CompleteBinaryTree (completeBinaryTree) where

import Data.List (sort)

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

completeBinaryTree :: [Int] -> [Int]
completeBinaryTree ns = let
    io = treeToInorder $ makeCBT [1..length ns]
    in map snd $ sort $ zip io ns

makeCBT :: [Int] -> Tree Int
makeCBT ns = cbinTree 1 (length ns) where
    cbinTree :: Int -> Int -> Tree Int
    cbinTree i lim
        | i <= lim  = Branch (ns !! (i-1)) (cbinTree (2*i) lim) (cbinTree (2*i+1) lim)
        | otherwise = Empty

treeToInorder :: Tree Int -> [Int]
treeToInorder Empty = []
treeToInorder (Branch i l r) = treeToInorder l ++ i : treeToInorder r

treeToPreorder :: Tree Int -> [Int]
treeToPreorder Empty = []
treeToPreorder (Branch i l r) = i : treeToPreorder l ++ treeToPreorder r

treeToPostorder :: Tree Int -> [Int]
treeToPostorder Empty = []
treeToPostorder (Branch i l r) = treeToPostorder l ++ treeToPostorder r ++ [i]


{-
Branch 0 
    (Branch 1 
        (Branch 3 
            (Branch 7 Empty Empty) 
            (Branch 8 Empty Empty)) 
        (Branch 4 
            (Branch 9 Empty Empty) Empty)) 
    (Branch 2 
        (Branch 5 Empty Empty) 
        (Branch 6 Empty Empty))

0,1,2,3,4,5,6,7,8,9
7,3,8,1,9,4,0,5,2,6



-}