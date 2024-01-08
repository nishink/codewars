-- https://www.codewars.com/kata/6408ba54babb196a61d66a65
-- The Bee

module TheBee (theBee) where

import Control.Monad (forM_)
import Data.Array (elems, (!))
import Data.Array.ST
    ( readArray, writeArray, MArray(newArray), runSTArray )

import Debug.Trace

theBee :: Integer -> Integer
theBee n = makeArray n ! (size,size) where size = n * 2 - 1

makeArray n = runSTArray $ do
    let size = n * 2 - 1
    arr <- newArray ((0,0),(size,size)) (0 :: Integer)   -- arr[0..size][0..size]の配列を作成
    writeArray arr (0,0) 1        -- arr[0][0] = 1
    forM_ [0..size-1] $ \y -> do
        let begin = if y < n then 0 else y-n+1
        let end = if y < n then n+y else size
        forM_ [begin..end-1] $ \x -> do
            let (y',x') = trace (show (y,x)) (y,x)
            cval <- readArray arr (y',x')
            rval <- readArray arr (y,x+1)
            rdval <- readArray arr (y+1,x+1)
            dval <- readArray arr (y+1,x)
            writeArray arr (y,x+1) (rval+cval)
            writeArray arr (y+1,x+1) (rdval+cval)
            writeArray arr (y+1,x) (dval+cval)
    return arr

{-
６角形のフィールドをどう表現するか。
例えば下記で表す。

  0 1 2 3 4
0|A|0|0|x|x| 
1|0|0|0|0|x|
2|0|0|0|0|0|
3|x|0|0|0|0|
4|x|x|0|0|B|

(0,0)から移動できる場所に1を加算する。
  0 1 2 3 4
0|A|1|0|x|x| 
1|1|1|0|0|x|
2|0|0|0|0|0|
3|x|0|0|0|0|
4|x|x|0|0|B|

(0,1)から移動できる場所に1を加算する。
  0 1 2 3 4
0|A|1|1|x|x| 
1|1|2|1|0|x|
2|0|0|0|0|0|
3|x|0|0|0|0|
4|x|x|0|0|B|

(0,2)から移動できる場所に1を加算する。
  0 1 2 3 4
0|A|1|1|x|x| 
1|1|2|2|1|x|
2|0|0|0|0|0|
3|x|0|0|0|0|
4|x|x|0|0|B|

(0,3),(0,4)はフィールド外なのでスキップ。
(1,0)から移動できる場所に1を加算する。
  0 1 2 3 4
0|A|1|1|x|x| 
1|1|3|2|1|x|
2|1|1|0|0|0|
3|x|0|0|0|0|
4|x|x|0|0|B|

(1,1)から移動できる場所に3((1,1)の値)を加算する。
  0 1 2 3 4
0|A|1|1|x|x| 
1|1|3|5|1|x|
2|1|4|3|0|0|
3|x|0|0|0|0|
4|x|x|0|0|B|

これを繰り返すことで、(4,4)に入っている値が答えになる。

フィールドのサイズは、n*2-1になるので、まずこのサイズのリストを用意する。
最初は0で初期化する。
0[0,0,0,0,0]
1[0,0,0,0,0]
先頭に1を入れる。
0[1,0,0,0,0]
1[0,0,0,0,0]
0行目について移動先に値を加算する。
0[1,1,1,0,0]
1[1,2,2,1,0]
行カウンタを増やす。次の行は0で初期化。
1[1,2,2,1,0]
2[0,0,0,0,0]
1行目について移動先に値を加算する。
1[1,3,5,6,0]
2[1,4,8,11,6]
-}