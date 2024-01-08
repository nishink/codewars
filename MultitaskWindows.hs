-- https://www.codewars.com/kata/5a24fff71f7f7051bd000097
-- Simple Fun #385: Multitask Windows

module MultitaskWindows.Kata (isValid) where

import Control.Monad (forM_)
import Data.Array (elems, (!))
import Data.Array.ST
    ( readArray, writeArray, MArray(newArray), runSTArray )

import Data.Char (digitToInt, isDigit)
import Data.List (permutations, nub)

isValid :: String -> Bool
isValid s = elem (stringToList s) allPattern
 
stringToList = map digitToInt . filter isDigit

allPattern = nub [ arrayToList $ makeArray s | s <- permutations [1..9] ]

arrayToList arr = [ arr ! (x,y) | y <- [0..3], x <- [0..3] ]

makeArray s = runSTArray $ do
    arr <- newArray ((0,0),(3,3)) 0   -- arr[0..3][0..3]の配列を初期値0で作成
    forM_ [0..8] $ \i -> do
        case (s !! i) of
            1 -> do
                writeArray arr (0,0) 1
                writeArray arr (0,1) 1
                writeArray arr (1,0) 1
                writeArray arr (1,1) 1
                return arr
            2 -> do
                writeArray arr (1,0) 2
                writeArray arr (1,1) 2
                writeArray arr (2,0) 2
                writeArray arr (2,1) 2
                return arr
            3 -> do
                writeArray arr (2,0) 3
                writeArray arr (2,1) 3
                writeArray arr (3,0) 3
                writeArray arr (3,1) 3
                return arr
            4 -> do
                writeArray arr (0,1) 4
                writeArray arr (0,2) 4
                writeArray arr (1,1) 4
                writeArray arr (1,2) 4
                return arr
            5 -> do
                writeArray arr (1,1) 5
                writeArray arr (1,2) 5
                writeArray arr (2,1) 5
                writeArray arr (2,2) 5
                return arr
            6 -> do
                writeArray arr (2,1) 6
                writeArray arr (2,2) 6
                writeArray arr (3,1) 6
                writeArray arr (3,2) 6
                return arr
            7 -> do
                writeArray arr (0,2) 7
                writeArray arr (0,3) 7
                writeArray arr (1,2) 7
                writeArray arr (1,3) 7
                return arr
            8 -> do
                writeArray arr (1,2) 8
                writeArray arr (1,3) 8
                writeArray arr (2,2) 8
                writeArray arr (2,3) 8
                return arr
            9 -> do
                writeArray arr (2,2) 9
                writeArray arr (2,3) 9
                writeArray arr (3,2) 9
                writeArray arr (3,3) 9
                return arr
    return arr


{-
4x4の画面に、2x2のウィンドウ９個を表示したとき、
見え方としてあり得る形ならTrue、そうでなければFalse。

ウインドウをクリックする順番は順列なので、
9*8*7*6*5*4*3*2=362880通り。
ただ、例えば1,3,7,9のウィンドウを最後にクリックすると、
他のウィンドウは隠れてしまうので、
結果の組み合わせって実は少ないのではないかと思う。

実際に全ての組み合わせの結果を出力してみたらどうだろう。
4x4のリストに、最初は0を入れておき、
1~9のウィンドウを順にクリックした結果を反映する。

実際に計算すると、結果の種類は2431パターンしかない。
事前に全てのパターンを計算しておいて、
一致するかどうかを見るだけで現実的な時間内で解を求めることができる。
-}