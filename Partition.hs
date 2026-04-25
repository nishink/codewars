-- https://www.codewars.com/kata/55cf3b567fc0e02b0b00000b
-- Getting along with Integer Partitions

-- この問題はまだ解けていない　stats 3 でスタックオーバーフローが起きる

module Codewars.G964.Partition where

import Control.Monad
import Data.List (nub, sort, foldl')
import Text.Printf (printf)
import qualified Data.Map as M

-- Memoizationを使ったパーティション列挙
partitionProducts :: Int -> Int -> M.Map (Int, Int) [[Int]] -> ([[Int]], M.Map (Int, Int) [[Int]])
partitionProducts 0 _ memo = ([[]], memo)
partitionProducts n maxElem memo = case M.lookup (n, maxElem) memo of
    Just res -> (res, memo)  -- メモ化されている場合、その結果を再利用
    Nothing  -> let (partitions, newMemo) = foldl' step ([], memo) [1..maxElem]
                    res = concat partitions
                in (res, M.insert (n, maxElem) res newMemo)  -- 計算結果をメモ化
  where
    step (parts, currentMemo) x = 
        let (subparts, updatedMemo) = partitionProducts (n - x) x currentMemo
        in (map (x :) subparts : parts, updatedMemo)

-- すべての分割に対する積を求める
products :: Int -> [Int]
products n = nub . sort $ map product partitions
  where (partitions, _) = partitionProducts n n M.empty  -- メモ化を使ってパーティションを生成

-- 範囲、平均、中央値を計算して文字列として返す関数
stats :: Int -> String
stats n = let prods = products n
              range = last prods - head prods
              avg   = (fromIntegral (sum prods) :: Double) / (fromIntegral (length prods) :: Double)
              med   = median prods
          in printf "Range: %d Average: %.2f Median: %.2f" range avg med

-- 中央値を計算する補助関数
median :: [Int] -> Double
median xs
  | odd len   = fromIntegral (xs !! mid)
  | otherwise = fromIntegral (xs !! (mid - 1) + xs !! mid) / 2
  where len = length xs
        mid = len `div` 2

-- 実行例
main :: IO ()
main = do
    print $ stats 3
    print $ stats 5
    print $ stats 8
    print $ stats 50  -- 大きな数値にも対応
