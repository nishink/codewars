-- https://www.codewars.com/kata/55f5efd21ad2b48895000040
-- How Many Numbers? II

module Codewars.G964.Maxsumdig where

maxSumDig :: Integer -> Int -> (Int, Integer, Integer)
maxSumDig nmax mxsm = 
    let
        validNumbers = filter (isValid mxsm) [1000..nmax]
        count = length validNumbers
        totalSum = sum validNumbers
        average = if count > 0 then fromIntegral totalSum / fromIntegral count else 0
        minDiff = minimum $ map (\x -> abs (fromIntegral x - average)) validNumbers
        closestToAverage = minimum $ filter (\x -> abs (fromIntegral x - average) == minDiff) validNumbers
    in
        (count, closestToAverage, totalSum)

-- 連続する4桁の合計がmaxSum以下か判定
isValid :: Int -> Integer -> Bool
isValid maxSum n =
    let s = show n
        ds = map (read . (:[])) $ replicate (6 - length s) '0' ++ s
        sums = [sum $ take 4 $ drop i ds | i <- [0..(length ds - 4)]]
    in all (<= maxSum) sums


{-
説明:

1000以上の数のうち、「連続する4桁の合計が指定された値を超えない」ものを探したいと考えています。
もし数値 num = d1d2d3d4d5d6 であり、4つの連続した桁の最大合計値が maxSum だとすると、
次の条件を満たす必要があります。

d1 + d2 + d3 + d4 <= maxSum
d2 + d3 + d4 + d5 <= maxSum
d3 + d4 + d5 + d6 <= maxSum

この目的のために、max_sumDig() という関数を作成します。
この関数は nMax（調査する区間の最大値、範囲は (1000, nMax)）と、
maxSum（連続する4桁の最大合計値）を受け取ります。関数は以下のデータを含むリストを出力してください：

[(1), (2), (3)]

(1) - 上記の条件を満たす数の個数

(2) - 結果の平均値に最も近い数（複数ある場合は最小のもの）

(3) - 見つかったすべての数の合計

具体例を見てみましょう：

max_sumDig(2000, 3) -------> [11, 1110, 12555]

(1) - 条件を満たす数は11個あります: 1000, 1001, 1002, 1010, 1011, 1020, 1100, 1101, 1110, 1200, 2000

(2) - これらの数の平均値は (1000 + 1001 + 1002 + 1010 + 1011 + 1020 + 1100 + 1101 + 1110 + 1200 + 2000) / 11 = 1141.36363
なので、平均値に最も近い数は1110です。

(3) - 見つかったすべての数の合計は12555です。 1000 + 1001 + 1002 + 1010 + 1011 + 1020 + 1100 + 1101 + 1110 + 1200 + 2000 = 12555

他の例も見てみましょう：

max_sumDig(2000, 4) -----> [21, 1120, 23665]

max_sumDig(2000, 7) -----> [85, 1200, 99986]

max_sumDig(3000, 7) -----> [141, 1600, 220756]



-}
