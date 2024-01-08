-- Square sums (simple)
-- https://www.codewars.com/kata/5a6b24d4e626c59d5b000066/haskell

module SquareSumsRow (squareSumsRow) where

import Data.List
import Distribution.Compat.Time (posixSecondsToModTime)

squareSumsRow :: Int -> Maybe [Int]
squareSumsRow n = let
    rs = [ result | i <- [1..n], let result = try n i [1..n] [], result /= [] ]
    in if null rs then Nothing else Just (head rs)

-- スクエアナンバー（整数の二乗、ただし１は除く）
-- この問題ではNの上限が43なので9x9=81が最大となる
squareNums :: [Int]
squareNums = [4,9,16,25,36,49,64,81]

-- 隣接する数字の候補
getNextCandidate :: Int -> Int -> [Int]
getNextCandidate n i = 
    filter (\x -> x > 0 && x <= n) $ map (+ (-i)) squareNums

-- これだとタイムアウトする
try :: Int -> Int -> [Int] -> [Int] -> [Int]
try n i [] used = used
try n i unused used = let
    xs = getNextCandidate n i \\ used
    rs = [ result | x <- xs, let result = try n x (unused \\ [x]) (x : used), result /= [] ]
    in if null rs then [] else head rs

-- 隣接する数字の候補を足していく。隣接するものがない場合はxsが[]になるので結果的に消える。
route :: Int -> [[Int]] -> [[Int]]
route n ps = 
    concat [ [ x : p | x <- xs ] | p <- ps, let xs = getNextCandidate n (head p) \\ p ]

squareSumsRow' :: Int -> Maybe [Int]
squareSumsRow' n = f n (group [1..n])
    where
        f 1 ps = if null ps then Nothing else Just (head ps)
        f i ps = f (i-1) (route n ps)



{-
N = 2 .. 43
なので、隣り合う数の合計は
2x2=4から9x9=81までの組み合わせとなる。

4 = 1,3
9 = 1,8 2,7 3,6 4,5
16 = 1,15 2,14 ... 7,9
25 = 1,24 2,23 ... 12,13
36 = 1,35 2,34 ... 17,19
49 = 6,43 7,42 ... 24,25
64 = 21,43 20,42 ... 31,33
81 = 38,43 39,42 40,41

スタートを１とすると、次に来るのは3,8,15,24,35のどれかしかない。
そうやって、次に来る候補を試していって、最後まで行ったらOK。


-}


----------
-- 1's, 0's and wildcards
-- https://www.codewars.com/kata/588f3e0dfa74475a2600002a/haskell
possibilities param = f param [""]
    where
        f [] result = result
        f (x:xs) result = let
            r = if x == '?' then map (++ "0") result ++ map (++ "1") result
                else map (++ [x]) result
            in f xs r

