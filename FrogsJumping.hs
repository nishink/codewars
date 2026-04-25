-- https://www.codewars.com/kata/58fff63f4c5d026cc200000f
-- Simple Fun #211: Frog's Jumping

module Kata.FrogsJumping (frog'sJumping) where

import Data.List
import Data.Maybe

frog'sJumping :: [Int] -> String
frog'sJumping stones = shortestPath stones

-- 最短のジャンプ列を計算する関数
shortestPath :: [Int] -> String
shortestPath stones = dp !! (length stones - 1)
  where
    n = length stones
    -- dp: 石ごとの最短経路を保持するリスト
    dp = go (replicate n "") [0] -- 初期化: 全て空文字列、最初は石0からスタート
    go dp [] = dp -- キューが空になったら終了
    go dp (i:queue) = go dp' queue'
      where
        currentPath = dp !! i -- 現在の石までのジャンプ列
        validIndices = catMaybes $
            map (flip findIndex stones . (==)) [stones !! i + 1, stones !! i + 2]
        queue' = foldl (\q j -> if dp !! j == "" then q ++ [j] else q) queue validIndices
        dp' = foldl updateDP dp validIndices
        updateDP d j =
          let nextPath = currentPath ++ jumpType j
              oldPath = d !! j
          in if oldPath == "" || betterPath nextPath oldPath then replace d j nextPath else d
        betterPath new old =
          length new < length old || (length new == length old && new < old)
        jumpType j
          | stones !! j == stones !! i + 1 = "1"
          | stones !! j == stones !! i + 2 = "2"

-- リストの特定のインデックスを値で置き換える関数
replace :: [a] -> Int -> a -> [a]
replace xs i x = take i xs ++ [x] ++ drop (i + 1) xs


{-
カエルのジャンプ。
カエルは１または２の距離をジャンプできる。
いくつかの石が置いてあるので、ジャンプできるパターンのうち、
数値として一番小さいものを求める。

例：
stones = [0,1,2,3,5,6]の場合、出力は"1221"である必要があります。
ここにすべての可能な道があります：
"11121"、最短のものではなく、
"2121"、最短のものの1つですが、辞書学上最小ではありません。
"1221"、最も短く、辞書学的に最小。

解き方としては、ひとまず全てのパターンを網羅した上で、
最短かつ辞書順で小さいものをソートで求めれば良い。

石を一つ取り出して、１つまたは２つ先に石があるかどうかを見る。


このやり方だとタイムアウトしてしまうので、考え方を変える。
短いジャンプと長いジャンプの両方ができる場合は、長いジャンプをする。
ただし、長いジャンプをした結果、次は短いジャンプしかできない場合は、
短いジャンプをしてから長いジャンプをする。

0,1,2,3,5という並びの時が、長いジャンプをした結果、次は短いジャンプしかできないパターン。
ただ、これ以外にも1,2,3,4,5,6,7,9みたいな場合も該当するようだ。

先の先を読んでいかないといけないのでは効率が悪い。
前のものとの差が１しかないものを取り出して、
奇数なら"1222..."、偶数なら"222..."としてはどうか。


結局考えてもわからなかったのでChatGPTに作ってもらった。

-}

{-
import Data.List
import Data.Ord

frog'sJumping :: [Int] -> String
frog'sJumping stones = head $ sortBy (comparing length <> comparing id) $ jumps stones [""]

jumps :: [Int] -> [String] -> [String]
jumps [] acc = acc
jumps (s:ss) acc = let
    canShortJump = elem (s+1) ss 
    canLongJump = elem (s+2) ss
    shortJump = if canShortJump then map (++"1") acc else []
    longJump = if canLongJump then map (++"2") acc else []
    in if null ss then acc else jumps ss shortJump ++ jumps (dropWhile (<s+2) ss) longJump
-}
