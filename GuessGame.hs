-- https://www.codewars.com/kata/5c607bb95a40bc7b4b9ecc27
-- Guess the number - Monad Edition

module GuessGame (guess) where

import Data.Bits
import Control.Monad.Reader

guess :: Monad m => (Int -> m Bool) -> m Int
guess gt = do -- if answer is 50
  let n1 = 64
  q1 <- gt n1 -- False
  let n2 = if q1 then n1 + 32 else n1 - 32 -- 64-32=32
  q2 <- gt n2 -- True
  let n3 = if q2 then n2 + 16 else n2 - 16 -- 32+16=48
  q3 <- gt n3 -- True
  let n4 = if q3 then n3 + 8 else n3 - 8 -- 48+8=56
  q4 <- gt n4 -- False
  let n5 = if q4 then n4 + 4 else n4 - 4 -- 56-4=52
  q5 <- gt n5 -- False
  let n6 = if q5 then n5 + 2 else n5 - 2 -- 52-2=50
  q6 <- gt n6 -- False
  let n7 = if q6 then n6 + 1 else n6 - 1 -- 50-1=49
  q7 <- gt n7 -- True
  return $ if q7 then n7 + 1 else n7

{-
guess gt = do
    q1 <- asks ((gt 0) . (.&. 1))
    q2 <- asks ((gt 0) . (.&. 2))
    q3 <- asks ((gt 0) . (.&. 4))
    q4 <- asks ((gt 0) . (.&. 8))
    q5 <- asks ((gt 0) . (.&. 16))
    q6 <- asks ((gt 0) . (.&. 32))
    q7 <- asks ((gt 0) . (.&. 64))
    f (b, n) = if b then n else 0
    return $ sum $ map f [(q1,1),(q2,2),(q3,4),(q4,8),(q5,16),(q6,32),(q7,64)]
-}

greaterThan :: Int -> Reader Int Bool
greaterThan n = asks (> n)

runGame :: Int -> Int
runGame n = runReader (guess greaterThan) n
