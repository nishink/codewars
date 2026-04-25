-- https://www.codewars.com/kata/638b042bf418c453377f28ad
-- Digits

module Digits (game) where

import Control.Monad.State

type Code = [Int]
type GameState = [Code] -- 候補のリストを状態として保持

-- すべての可能なコードを生成
allCodes :: [Code]
allCodes = [[a, b, c, d] | a <- [0..9], b <- [0..9], c <- [0..9], d <- [0..9], a /= b, a /= c, a /= d, b /= c, b /= d, c /= d]

-- マッチ数を計算
matches :: Code -> Code -> Int
matches guess code = length $ filter id $ zipWith (==) guess code

-- ゲームロジック
game :: Int -> State GameState [Int]
game (-1) = do
  -- 初回の推測
  put allCodes
  return [0, 1, 2, 3] -- 初期の推測（任意の4桁の異なる数字）
game n = do
  -- 状態から候補を取得
  candidates <- get
  let lastGuess = head candidates
  -- 候補を絞り込む
  let newCandidates = filter (\code -> matches lastGuess code == n) candidates
  put newCandidates
  -- 次の推測を返す
  return (head newCandidates)
