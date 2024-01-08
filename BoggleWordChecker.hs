-- https://www.codewars.com/kata/57680d0128ed87c94f000bfd
-- Boggle Word Checker

module BoggleWordChecker (findWord) where

import Data.List

findWord :: [[Char]] -> [Char] -> Bool
findWord board (w:ord) = let
    ps = findPos board w
    in if null ps then False else any (\p -> findRecursive board ord p []) ps

getSize :: [[Char]] -> (Int,Int)
getSize board = (length $ head board, length board)

findPos :: [[Char]] -> Char -> [(Int,Int)]
findPos board chara = let
    (w,h) = getSize board
    in [ (x,y) | y <- [0..h-1], x <- [0..w-1], board !! y !! x == chara ]

neighbours :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
neighbours (w,h) (x,y) = 
    filter (\(a,b) -> a >= 0 && b >= 0 && a < w && b < h)
        [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]

findNeighbour :: [[Char]] -> Char -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
findNeighbour board chara pos used = let
    size = getSize board
    in [ p | p@(x,y) <- neighbours size pos \\ used, board !! y !! x == chara ]

findRecursive :: [[Char]] -> [Char] -> (Int,Int) -> [(Int,Int)] -> Bool
findRecursive _ [] _ _ = True
findRecursive board (w:ord) pos used = let
    ns = findNeighbour board w pos used
    in if null ns then False else any (\p -> findRecursive board ord p (pos:used)) ns


sample = 
    [ "ILAW"
    , "BNGE"
    , "IUAO"
    , "ASRL"
    ]

{-
boardの中で縦横斜めに隣接する文字をつなげて
与えられたwordができるかどうかをチェックする。

例えば以下のような解き方はどうか。
・wordの最初の文字をboardの中から探し、見つかった座標を記録する。
・見つかった座標に隣接する文字の中に、wordの次の文字が含まれているか探す。
　見つからなければ終了し、次の座標を探す。
・見つかった場合は座標を記録し、さらにそこから隣接する座標を探す。
　この時、一度使った座標は除外する。
・複数見つかる場合があるので、見つかった分だけ探す。


-}