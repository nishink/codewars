-- https://www.codewars.com/kata/57e5279b7cf1aea5cf000359
-- Fun with trees: max sum

module MaxSum (maxSum) where

-- import MaxSumPreload
-- defined in MaxSumPreload
data TreeNode = Node TreeNode Int TreeNode | Leaf Int | None

maxSum :: TreeNode -> Int
maxSum None = 0
maxSum (Leaf value) = value
maxSum (Node left value right) = value + max (maxSum left) (maxSum right)
