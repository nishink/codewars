-- https://www.codewars.com/kata/5c2d5de69611562191289041
-- Kata 2019: Bonus Game I

module Main where
--module BonusGame (calc) where

import Data.List
import Data.Array.Unboxed

calc :: [Int] -> Int
calc cards = maximum $ map score (candidates cards)

candidates :: [Int] -> [[Int]]
candidates [] = [[]]
candidates [x] = [[x]]
candidates [x,y] = [[x,y],[y,x]]
candidates cards = let
    h = head cards
    t = tail cards
    l = last cards
    i = init cards
  in map (h:) (candidates t) ++ map (l:) (candidates i)

bonus = [ 2^n | n <- [1..] ]
score cards = sum $ zipWith (*) bonus cards

calcs :: [Int] -> [Int]
calcs [] = []
calcs [x] = [x]
calcs [x,y] = if x < y then [x,y] else [y,x]
calcs cards@(h:t) = let
    l = last cards
    i = init cards
    ts = calcs t
    is = calcs i
  in
    if h + 2 * score ts > l + 2 * score is
      then h : ts
      else l : is

calcscore :: Int -> [Int] -> Int
calcscore 0 [] = 0
calcscore 1 [x] = 2 * x
calcscore 2 [x,y] = 2 * max (x + 2 * y) (y + 2 * x)
calcscore 3 [x,y,z] = 2 * max (x + calcscore 2 [y,z]) (z + calcscore 2 [x,y])
calcscore 4 [w,x,y,z] = 2 * max (w + calcscore 3 [x,y,z]) (z + calcscore 3 [w,x,y])
calcscore 5 [v,w,x,y,z] = 2 * max (v + calcscore 4 [w,x,y,z]) (z + calcscore 4 [v,w,x,y])
calcscore 6 [u,v,w,x,y,z] = 2 * max (u + calcscore 5 [v,w,x,y,z]) (z + calcscore 5 [u,v,w,x,y])
calcscore 7 [t,u,v,w,x,y,z] = 2 * max (t + calcscore 6 [u,v,w,x,y,z]) (z + calcscore 6 [t,u,v,w,x,y])
calcscore 8 [s,t,u,v,w,x,y,z] = 2 * max (s + calcscore 7 [t,u,v,w,x,y,z]) (z + calcscore 7 [s,t,u,v,w,x,y])
calcscore 9 [r,s,t,u,v,w,x,y,z] = 2 * max (r + calcscore 8 [s,t,u,v,w,x,y,z]) (z + calcscore 8 [r,s,t,u,v,w,x,y])
calcscore 10 [q,r,s,t,u,v,w,x,y,z] = 2 * max (q + calcscore 9 [r,s,t,u,v,w,x,y,z]) (z + calcscore 9 [q,r,s,t,u,v,w,x,y])

calcscore len cards@(h:t) = 2 * (max ts is)
  where
    len' = len-1
    l = last cards
    i = take len' cards
    ts = h + calcscore len' t
    is = l + calcscore len' i

--calcscore len cards@(h:t) = 2 * ( max (h + calcscore (len-1) t) (last cards + calcscore (len-1) (take (len-1) cards)) )

calcr :: [Int] -> [Int] -> Int
calcr []    _ = 0
calcr [x]   _ = 2 * x
calcr [x,y] _ = 2 * max (x + 2 * y) (y + 2 * x)
calcr cards@(h:t) rcards@(l:i) = 2 * (max ts is)
  where
    ts = h + calcr t (reverse t)
    is = l + calcr i (reverse i)

initLast :: [a] -> ([a], a)
initLast xs = (init xs, last xs)

initLast' :: [a] -> ([a], a)
initLast' [x] = ([], x)
initLast' (x:xs) = let (xs', y) = initLast' xs
                   in (x:xs', y)

calci :: [Int] -> Int
calci []    = 0
calci [x]   = 2 * x
calci [x,y] = 2 * max (x + 2 * y) (y + 2 * x)
calci cards@(h:t) = 2 * (max ts is)
  where
    (i, l) = initLast cards
    ts = h + calci t
    is = l + calci i

calct :: (Int,Int) -> [Int] -> Int
calct (h,l) cards
  | h == l = 2 * (cards !! h)
  | otherwise = 2 * (max ts is)
  where
    hp = cards !! h
    lp = cards !! l
    ts = hp + calct (h+1,l) cards
    is = lp + calct (h,l-1) cards

calca :: (Int,Int) -> UArray Int Int -> Int
calca (h,l) cards
  | h == l = 2 * (cards ! h)
  | otherwise = 2 * (max ts is)
  where
    hp = cards ! h
    lp = cards ! l
    ts = hp + calca (h+1,l) cards
    is = lp + calca (h,l-1) cards








calcp :: [Int] -> [Int]-> Int -> Int
calcp cards res 0 = head res
calcp cards res n = let
    len = length cards
    k = len - n
  in calcp cards [ 2 * max (cards !! i + res !! (i+1)) (cards !! (i+k) + res !! i) | i <- [0..len-k] ] (n-1)

main = print $ calcp cards (replicate (length cards + 1) 0) (length cards)
  where
    cards = [1..30]

{-
def calc(a):
    res = [0] * (len(a) + 1)
    for k in range(len(a)):
        res = [2 * max(a[i] + res[i+1], a[i+k] + res[i]) for i in range(len(a) - k)]
    return res[0]
-}







{-
import Data.Vector
calcvec :: Vector Int -> Int
calcvec Empty = 0
calcvec vec = 2 * (max ts is)
  where
    h = head vec
    t = tail vec
    l = last vec
    i = init vec
    ts = h + calcvec t
    is = l + calcvec i

main = print $ calcvec $ fromList [4, 10, 2, 3, 1, 3, 1, 6, 9]
-}

--main = print $ calcscore [4, 10, 2, 3, 1, 3, 1, 6, 9]
--main = print $ calcscore (length cards) cards
--main = print $ calcr cards (reverse cards)
--main = print $ calci cards
--main = print $ calct (0, length cards - 1) cards
--main = print $ calca (0, length cards - 1) (listArray (0, length cards - 1) cards)



{-
ghc -O -rtsopts -prof -fprof-auto BonusGame.hs
./BonusGame +RTS -p
-}

