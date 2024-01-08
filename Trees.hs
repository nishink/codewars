-- https://www.codewars.com/kata/5f148fcbe2c2e0002ec7c20e
-- Keep the balance

module Trees where

-- import Preloaded --
--import Test.QuickCheck
import qualified Data.Tree as T

-- Data Structure
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a) deriving Eq

-- Testing functions
getHeight Leaf = 0
getHeight (Node h l x r) = 1 + max (getHeight l) (getHeight r)

isBalancedTree :: Tree a -> Bool
isBalancedTree Leaf = True
isBalancedTree (Node h left x right) =
  abs (getHeight left - getHeight right) <= 1
  &&
  isBalancedTree left
  &&
  isBalancedTree right

count Leaf = 0
count (Node _ r _ l) = 1 + count r + count l

-- Debugging
toDataTree :: Show a => Tree a -> T.Tree String
toDataTree Leaf = T.Node "leaf" []
toDataTree (Node height left x right) =
  T.Node (show height ++ ", " ++ show x ) [toDataTree left, toDataTree right]

instance Show a => Show (Tree a) where
  show tree = T.drawTree (toDataTree tree)

-- Kata --
insertBalanced :: Tree a -> a -> Tree a
insertBalanced Leaf i = Node 1 Leaf i Leaf
insertBalanced (Node h Leaf x Leaf) i = Node (h+1) (Node 1 Leaf i Leaf) x Leaf
insertBalanced (Node h Leaf x r) i = Node h (Node 1 Leaf i Leaf) x r 
insertBalanced (Node h l x Leaf) i = Node h l x (Node 1 Leaf i Leaf) 
insertBalanced (Node h l@(Node lh ll lx lr) x r@(Node rh rl rx rr)) i =
    if lh < rh 
        then let
            li@(Node lih lil lix lir) = insertBalanced l i
            in Node (lih+1) li x r 
        else let
            ri@(Node rih ril rix rir) = insertBalanced r i
            in Node (rih+1) l x ri
{-
バランスをとって要素を追加するには、左右の高さが低い方に追加する。
何もない状態なら、高さ１のNodeを追加すればよい。
Nodeがあって、片方がLeafなら、Leafの位置に高さ1のNodeを追加する。
Nodeがあって、両方Leafなら、そのNodeの高さを＋１した上で、左側にNodeを追加する。
Nodeがあって、両方Nodeなら、高さの低い方にNodeを追加する。
Nodeを追加したことで高さが変わるなら、自身の高さも＋１する。
-}
