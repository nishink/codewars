-- https://www.codewars.com/kata/5a6de0ec0136a1761d000093
-- The Binary Tree, or There and Back Again

module ThereAndBackAgain (rotateLeft,rotateRight) where

data Tree a = Empty | Node { left, right :: Tree a , value :: a } deriving (Show,Eq,Foldable)

rotateLeft,rotateRight :: Eq a => Tree a -> Tree a

-- 旧ルートの右の子を新ルートとし、旧ルートは新ルートの左の子にする。
-- 新ルートの元々の左の子は、旧ルートの右の子と入れ替える。
rotateLeft root =
    if root == Empty then root
    else if right root == Empty then root
    else let
        newroot = right root
        oldroot = Node {left = left root, right = left newroot, value = value root}
        in Node {left = oldroot, right = right newroot, value = value newroot}

-- 旧ルートの左の子を新ルートとし、旧ルートは新ルートの右の子にする。
-- 新ルートの元々の右の子は、旧ルートの左の子と入れ替える。
rotateRight root = 
    if root == Empty then root
    else if left root == Empty then root 
    else let
        newroot = left root
        oldroot = Node {left = right newroot, right = right root, value = value root}
        in Node {left = left newroot, right = oldroot, value = value newroot}


