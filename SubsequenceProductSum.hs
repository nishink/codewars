-- https://www.codewars.com/kata/5d653190d94b3b0021ec8f2b
-- Subsequence Product Sum

module SubsequenceProductSum (productSum) where

import Control.Monad (forM_)
import Data.Array (elems, (!))
import Data.Array.ST
    ( readArray, writeArray, MArray(newArray), runSTArray )

modulo :: Int
modulo = 10^9 + 7

productSum :: [Int] -> Int -> Int
productSum a m = fromIntegral $ makeArray a m ! m `mod` fromIntegral modulo 

makeArray a m = runSTArray $ do
    arr <- newArray (0,m) (0 :: Integer)   -- arr[0..m]の配列を作成
    writeArray arr 0 1        -- arr[0] = 1
    forM_ a $ \n -> do
        forM_ [m,m-1..1] $ \k -> do
            sk <- readArray arr k
            ss <- readArray arr (k-1)
            writeArray arr k (sk + fromIntegral n * ss)
    return arr

{-
部分文字列の乗算の総和を求める。

Pythonでの解き方を見つけたので、Haskellで実装しなおす。

def product_sum(a, m):
    ss = [1] + [0]*m ;
    for n in a:
        for k in range(m,0,-1):
            ss[k] += n * ss[k-1];
        
    return ss[m] 

[2,3,4,5]
3

ss = [1,0,0,0]
n = 2
  ss = [1,2,0,0]
n = 3
  ss = [1,5,6,0]
     = [1,2+3,2*3,0]
n = 4
  ss = [1,9,26,24]
     = [1,2+3+4,2*3+(2+3)*4,2*3*4]
n = 5
  ss = [1,14,71,154]
     = [1,2+3+4+5,{2*3+(2+3)*4}+(2+3+4)*5,2*3*4+{2*3+(2+3)*4}*5]

2*3*4+{2*3+(2+3)*4}*5 = 2*3*4 + 2*3*5 + 2*4*5 + 3*4*5 

-}
