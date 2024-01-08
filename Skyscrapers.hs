-- https://www.codewars.com/kata/5671d975d81d6c1c87000022
-- 4 By 4 Skyscrapers

module Haskell.Codewars.Skyscrapers where

import Data.List

type Clues  = [[Int]]
type Puzzle = [[Int]]

solve :: Clues -> Puzzle
solve [] = []
solve clues = head [ skyscrapers | 
    let perm1 = permutations [1..4], p1 <- perm1, 
    let perm2 = f p1 perm1, p2 <- perm2,
    let perm3 = f p2 perm2, p3 <- perm3, 
    let perm4 = f p3 perm3, p4 <- perm4,
    let skyscrapers = [p1,p2,p3,p4], check (concat clues) (map count $ seenFromClue skyscrapers) ]
    where
        f [a,b,c,d] perm = [ p | p@[w,x,y,z] <- perm, a/=w, b/=x, c/=y, d/=z ]

count :: [Int] -> Int
count [1,2,3,4] = 4
count [1,2,4,3] = 3
count [1,3,_,_] = 3
count [1,4,_,_] = 2
count [2,1,3,4] = 3
count [2,1,4,3] = 2
count [2,3,_,_] = 3
count [2,4,_,_] = 2
count [3,_,_,_] = 2
count [4,_,_,_] = 1
count _ = 0

seenFromClue :: [[Int]] -> [[Int]]
seenFromClue ss = 
    [ [ ss !! y !! x | y <- [0..3]] | x <- [0..3] ] -- 0..3
    ++ [ reverse (ss !! y) | y <- [0..3] ] -- 4..7
    ++ [ [ ss !! y !! x | y <- [3,2..0]] | x <- [3,2..0] ] -- 8..11
    ++ [ ss !! y | y <- [3,2..0] ] -- 12..15

check :: [Int] -> [Int] -> Bool
check clues skyscrapers =
    and [ c == 0 || c == s | i <- [0..15], let c = clues !! i, let s = skyscrapers !! i ]

clues1 :: Clues
clues1 = [[2, 2, 1, 3],  
          [2, 2, 3, 1],  
          [1, 2, 2, 3],  
          [3, 2, 1, 3]]

clues2 :: Clues
clues2 = [[0, 0, 1, 2],   
          [0, 2, 0, 0],   
          [0, 3, 0, 0], 
          [0, 1, 0, 0]]


{-
解き方
clueが1の時は高さ4がすぐ前にある。
clueが2の時は逆に高さ4はすぐ前にない。
clueが4の時は、手前から1,2,3,4で確定。

考え方
count:clueが見たときに幾つに見えるか

１つの行は1,2,3,4の組み合わせだが、
列も1,2,3,4の組み合わせになる必要がある。
1,2,3,4の組み合わせは16通りだが、
次の行の候補はだいぶ絞られる。

1,2,3,4
2,3,4,1
3,4,1,2
4,1,2,3


-}