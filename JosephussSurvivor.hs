-- https://www.codewars.com/kata/555624b601231dc7a400017a
-- Josephus Survivor

module Codewars.G964.Josephus where
josephusSurvivor :: Int -> Int -> Int
josephusSurvivor n k = f n k + 1
  where
    f 1 k = 0
    f n k = ((f (n-1) k) + k) `mod` n
    -- https://ja.wikipedia.org/wiki/ヨセフスの問題
    -- f(n,k)=(f(n-1,k)+k) mod n、ただしf(1,k)=0

