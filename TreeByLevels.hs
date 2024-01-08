-- https://www.codewars.com/kata/52bef5e3588c56132c0003bc
-- Sort binary tree by levels

module TreeByLevels where

import Data.List

-- import TreeByLevels.TreeNode

data TreeNode a = TreeNode {
  left  :: Maybe (TreeNode a),
  right :: Maybe (TreeNode a),
  value :: a
  } deriving Show

treeByLevels :: Maybe (TreeNode a) -> [a]

treeByLevels t = map snd $ sortOn fst $ tree 1 t
  where
    tree _ Nothing = []
    tree n (Just (TreeNode l r v)) = (n,v) : tree (n*2) l ++ tree (n*2+1) r

