-- https://www.codewars.com/kata/57e5a6a67fbcc9ba900021cd
-- Fun with trees: array to tree

module ArrayToTree (toTree) where

-- import Preloaded (TreeNode(..)) -- TreeNode = None | Node TreeNode Int TreeNode
import Data.Vector.Unboxed qualified as U

toTree :: U.Vector Int -> TreeNode
toTree ns = cbinTree 1 (U.length ns) where
    cbinTree :: Int -> Int -> TreeNode
    cbinTree i lim
        | i <= lim  = Node (cbinTree (2*i) lim) (ns U.! (i-1)) (cbinTree (2*i+1) lim)
        | otherwise = None
    

{-
ベクトル配列から二分木を作る。
以前解いたCompleteBinaryTreeの回答をそのまま適用できた。

-- https://www.codewars.com/kata/5c80b55e95eba7650dc671ea
-- Complete Binary Tree

makeCBT :: [Int] -> Tree Int
makeCBT ns = cbinTree 1 (length ns) where
    cbinTree :: Int -> Int -> Tree Int
    cbinTree i lim
        | i <= lim  = Branch (ns !! (i-1)) (cbinTree (2*i) lim) (cbinTree (2*i+1) lim)
        | otherwise = Empty


-}