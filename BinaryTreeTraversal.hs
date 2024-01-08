-- https://www.codewars.com/kata/5268956c10342831a8000135
-- Binary Tree Traversal

module BinaryTreeTraversal
  ( preOrder
  , inOrder
  , postOrder
  ) where

{-
import BinaryTreeTraversal.Types
-}

data Tree a = Nil | Node (Tree a) a (Tree a)

-- 1.) Root node, 2.) traverse left subtree, 3.) traverse right subtree.
preOrder :: Tree a -> [a]
preOrder Nil = []
preOrder (Node left value right) = [value] ++ (preOrder left) ++ (preOrder right)

-- 1.) Traverse left subtree, 2.) root node, 3.) traverse right subtree.
inOrder :: Tree a -> [a]
inOrder Nil = []
inOrder (Node left value right) = (inOrder left) ++ [value] ++ (inOrder right)

-- 1.) Traverse left subtree, 2.) traverse right subtree, 3.) root node.
postOrder :: Tree a -> [a]
postOrder Nil = []
postOrder (Node left value right) = (postOrder left) ++ (postOrder right) ++ [value]
