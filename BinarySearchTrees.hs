-- https://www.codewars.com/kata/571a551a196bb0567f000603
-- Binary Search Trees

module BinarySearchTrees () where

-- import Preloaded (Tree(Tree))
data Tree = Tree Node (Maybe Tree) (Maybe Tree) deriving ()
newtype Node = Node Char deriving (Eq,Ord)
instance Show Node where
    show (Node c) = show c


instance Show Tree where
    show (Tree node Nothing Nothing) = "[" ++ show node ++ "]"
    show (Tree node left right) = 
        "[" ++ f left ++ " " ++ show node ++ " " ++ f right ++ "]"
        where
            f Nothing = "_"
            f (Just tree) = show tree

instance Eq Tree where
    (Tree node left right) == (Tree node' left' right') =
        node == node' && left == left' && right == right'

{-
二分木を作る。
表現形式は以下。
"[_ B [C]]"

この問題では二分木のShowとEqを実装すればよい。
Showは[]で囲んで、左の枝、ノードの値、右の枝をそれぞれ出力。枝の先は再起。
Eqはノードの値、左の枝、右の枝をそれぞれ比較して一致しているかどうかで判定。
-}