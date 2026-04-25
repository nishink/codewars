-- https://www.codewars.com/kata/56f78a42f749ba513b00037f
-- Probabilities for Sums in Rolling Cubic Dice

module CubicDice (rolldiceSumProb) where

rolldiceSumProb :: Int -> Int -> Double
rolldiceSumProb sum' diceAmount = 
    let totalOutcomes = 6 ^ diceAmount
        waysToGetSum = countWays sum' diceAmount
    in fromIntegral waysToGetSum / fromIntegral totalOutcomes

countWays :: Int -> Int -> Int
countWays targetSum diceLeft
    | diceLeft == 0 = if targetSum == 0 then 1 else 0
    | targetSum < diceLeft || targetSum > diceLeft * 6 = 0
    | otherwise = sum [countWays (targetSum - face) (diceLeft - 1) | face <- [1..6]]



{-
引数に合計値とサイコロの数が与えられます。
各サイコロは1から6までの目が出る通常の立方体サイコロです。
rolldiceSumProb 関数は、指定された数のサイコロを振ったときに
指定された合計値が出る確率を計算して返します。
確率は小数で表され、0から1の範囲になります。

例えば、rolldiceSumProb 7 2 は、2つのサイコロを振ったときに合計が7になる確率を計算します。
この場合、2つのサイコロの組み合わせで合計が7になるのは6通りあり、
全ての組み合わせは36通り（6×6）なので、確率は6/36 = 1/6 ≈ 0.16666667となります。
ヒント:
動的計画法を使用して、各サイコロの目の組み合わせを効率的に計算することができます。

-}