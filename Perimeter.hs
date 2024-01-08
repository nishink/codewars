-- https://www.codewars.com/kata/559a28007caad2ac4e000083
-- Perimeter of squares in a rectangle

module Codewars.Kata.Perimeter where

perimeter :: Integer -> Integer
perimeter n = 4 * sum (take (fromIntegral (n + 1)) fib)

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)
