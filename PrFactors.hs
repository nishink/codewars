-- https://www.codewars.com/kata/54d512e62a5e54c96200019e
-- Primes in numbers

module Codewars.Kata.PrFactors where

import Data.List

factors p n
    | n == 1 = []
    | n < p^2 = [n]
    | (d, 0) <- divMod n p = p : factors p d
    | otherwise = factors (p + 1) n

{-
prime_factors :: Integer -> String  
prime_factors = concatMap format . group . flip factors 2
    where
        format x = "(" ++ show (head x) ++ 
            (if length x > 1 then "**" ++ show (length x) else "") ++ ")"
-}
-- |
-- Module      : Data.Numbers.Primes
-- Copyright   : Sebastian Fischer
-- License     : BSD3
-- 
-- Maintainer  : Sebastian Fischer (sebf@informatik.uni-kiel.de)
-- Stability   : experimental
-- Portability : portable
-- 
-- This Haskell library provides an efficient lazy wheel sieve for
-- prime generation inspired by /Lazy wheel sieves and spirals of/
-- /primes/ by Colin Runciman
-- (<http://www.cs.york.ac.uk/ftpdir/pub/colin/jfp97lw.ps.gz>) and
-- /The Genuine Sieve of Eratosthenes/ by Melissa O'Neil
-- (<http://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf>).
-- 
primes :: Integral int => [int]
primes = wheelSieve 6

wheelSieve :: Integral int => Int -> [int]
wheelSieve k = reverse ps ++ map head (sieve p (cycle ns))
 where (p:ps,ns) = wheel k

isPrime :: Integral int => int -> Bool
isPrime n | n > 1     = primeFactors n == [n]
          | otherwise = False

primeFactors :: Integral int => int -> [int]
primeFactors n = factors n (wheelSieve 6)
 where
  factors 1 _                  = []
  factors m (p:ps) | m < p*p   = [m]
                   | r == 0    = p : factors q (p:ps)
                   | otherwise = factors m ps
   where (q,r) = quotRem m p

sieve :: (Ord int, Num int) => int -> [int] -> [[int]]
sieve p ns@(m:ms) = spin p ns : sieveComps (p+m) ms (composites p ns)

type Composites int = (Queue int,[[int]])

composites :: (Ord int, Num int) => int -> [int] -> Composites int
composites p ns = (Empty, map comps (spin p ns : sieve p ns))
 where comps xs@(x:_) = map (x*) xs

splitComposites :: Ord int => Composites int -> (int,Composites int)
splitComposites (Empty, xs:xss) = splitComposites (Fork xs [], xss)
splitComposites (queue, xss@((x:xs):yss))
  | x < z     = (x, discard x (enqueue xs queue, yss))
  | otherwise = (z, discard z (enqueue zs queue', xss))
 where (z:zs,queue') = dequeue queue

discard :: Ord int => int -> Composites int -> Composites int
discard n ns | n == m    = discard n ms
             | otherwise = ns
 where (m,ms) = splitComposites ns

sieveComps :: (Ord int, Num int) => int -> [int] -> Composites int -> [[int]]
sieveComps cand ns@(m:ms) xs
  | cand == comp = sieveComps (cand+m) ms ys
  | cand <  comp = spin cand ns : sieveComps (cand+m) ms xs
  | otherwise    = sieveComps cand ns ys
 where (comp,ys) = splitComposites xs

spin :: Num int => int -> [int] -> [int]
spin x (y:ys) = x : spin (x+y) ys

type Wheel int = ([int],[int])

wheel :: Integral int => Int -> Wheel int
wheel n = iterate next ([2],[1]) !! n

next :: Integral int => Wheel int -> Wheel int
next (ps@(p:_),xs) = (py:ps,cancel (product ps) p py ys)
 where (y:ys) = cycle xs
       py = p + y

cancel :: Integral int => int -> int -> int -> [int] -> [int]
cancel 0 _ _ _ = []
cancel m p n (x:ys@(y:zs))
  | nx `mod` p > 0 = x : cancel (m-x) p nx ys
  | otherwise      = cancel m p n (x+y:zs)
 where nx = n + x

data Queue int = Empty | Fork [int] [Queue int]

enqueue :: Ord int => [int] -> Queue int -> Queue int
enqueue ns = merge (Fork ns [])

merge :: Ord int => Queue int -> Queue int -> Queue int
merge Empty y                        = y
merge x     Empty                    = x
merge x     y     | prio x <= prio y = join x y
                  | otherwise        = join y x
 where prio (Fork (n:_) _) = n
       join (Fork ns qs) q = Fork ns (q:qs)

dequeue :: Ord int => Queue int -> ([int], Queue int)
dequeue (Fork ns qs) = (ns,mergeAll qs)

mergeAll :: Ord int => [Queue int] -> Queue int
mergeAll []       = Empty
mergeAll [x]      = x
mergeAll (x:y:qs) = merge (merge x y) (mergeAll qs)

{-
Example: n = 86240 should return "(2**5)(5)(7**2)(11)"
-}