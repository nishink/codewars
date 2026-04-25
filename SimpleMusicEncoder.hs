-- https://www.codewars.com/kata/58db9545facc51e3db00000a
-- A Simple Music Encoder

module Encoder (compress) where

compress :: [Int] -> String
compress [] = ""
compress [n] = show n
compress ns = let
    (t, d) = splitAtBreak ns
    diff = interval ns
    first = show (head t)
    count = if diff == 0 then "*" ++ show (length t) else ""
    lastn = if diff /= 0 && length t >= 3 then "-" ++ show (last t) ++ (if abs diff == 1 then "" else "/" ++ (show $ abs diff)) else ""
    remain = if diff /= 0 && length t == 2 then tail ns else d
    in first ++ count ++ lastn ++ (if remain == [] then "" else "," ++ compress remain)
    
 -- 間隔を取得
interval :: [Int] -> Int
interval ns = head (zipWith (-) (tail ns) ns)

-- 数列を分割する関数
splitAtBreak :: [Int] -> ([Int], [Int])
splitAtBreak [] = ([], [])
splitAtBreak [x] = ([x], [])
splitAtBreak xs = go xs
    where
        diff = interval xs
        go (y1:y2:ys)
            | y2 - y1 == diff = let (same, rest) = go (y2:ys) in (y1:same, rest)
            | otherwise = ([y1], y2:ys)
        go ys = (ys, [])

{-
仕様
2つ以上の同一の数字のシーケンスは、number*count
3つ以上の連続した数字のシーケンスは、first-lastとして短縮されます。これは昇順と降順の両方に当てはまります
同じ間隔の3つ以上の数字のシーケンスは、first-last/intervalとして短縮されます。間隔は記号を必要としないことに注意してください
圧縮は左から右に起こります

例
[1, 3, 4, 5, 7, 8, 9, 10, 11, 14, 15, 17, 18, 19, 20] -> "1,3-5,7-11,14,15,17-20"
[0, 2, 4, 5, 7, 8, 9] -> "0-4/2,5,7-9"
[0, 2, 4, 5, 7, 6, 5] -> "0-4/2,5,7-5"
[0, 2, 4, 5, 7, 6, 5, 5, 5, 5, 5] -> "0-4/2,5,7-5,5*4"

解法
数字列を先頭から順に見ていく。
最初の２つが同じ数字なら、同じ数字が何個続くか調べ、number*countに変換する。
最初の３つが連続した数字（１つ目と２つ目の差、２つ目と３つ目の差が同じ）なら、
同じ条件がどこまで続くか調べ、first-lastに変化する。
差が2以上または-2以下の場合は/intervalをつける。

同じ条件がどこまで続くかを調べる方法はChatGPTに聞いた。
-}