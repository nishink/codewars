-- https://www.codewars.com/kata/52423db9add6f6fc39000354
-- Conway's Game of Life - Unlimited Edition

module UnlimitedGameOfLife where

{-

import Data.List

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration cells 0 = cells
getGeneration cells gen = let
    width = length $ head cells
    height = length cells
    wrap (x, y) = ((x `mod` width), (y `mod` height))
    neighbours (x, y) = 
        map wrap [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y),
                  (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
    getCell (x, y) = cells !! y !! x
    
    nextCells = [
        [ judge c n | x <- [0..width-1], let c = getCell (x, y),
                  let n = sum $ map getCell $ neighbours (x, y) ] |
                  y <- [0..height-1]]
    in getGeneration nextCells (gen-1)

judge :: Int -> Int -> Int
judge c n =
  if c == 1
    then if n == 2 || n == 3 then 1 else 0
    else if n == 3 then 1 else 0
-}

------------------------------------------------------

import Data.List

type Pos = (Int, Int)
type Board = [Pos]

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration cells gen = let
    board = cellsToBoard cells
    in boardToCells $ getGen board gen

getGen :: Board -> Int -> Board
getGen board 0 = board
getGen board gen = getGen (next board) (gen-1)

-- 生存判定：pがbの要素であれば生存と判定
isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

-- 死滅判定：生存の逆
isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

-- 周囲８方向を取得
neighbers :: Pos -> [Pos]
neighbers (x, y) = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y),
                    (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

-- 周囲に生きているセルの個数を取得
liveneighbers :: Board -> Pos -> Int
liveneighbers b = length . filter (isAlive b) . neighbers

-- 現在の状態から生存となるものを抽出
survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbers b p `elem` [2, 3]]

-- 現在の状態から誕生するものを抽出
births :: Board -> [Pos]
births b =
  [p | p <- nub (concatMap neighbers b), -- 今いるセルの隣人を取り出し重複を削除
       isEmpty b p, -- 現在は不在の状態
       liveneighbers b p == 3] -- 周囲に生きているセルが３つなら誕生と判定

-- 次の状態：現在の状態から生存判定になるものと誕生判定になるものを合体
next :: Board -> Board
next b = survivors b ++ births b

-- 変換
cellsToBoard :: [[Int]] -> Board
cellsToBoard cells = let
    width = length $ head cells
    height = length cells
    in [ (x, y) | x <- [0..width-1], y <- [0..height-1], cells !! y !! x == 1 ]

boardToCells :: Board -> [[Int]]
boardToCells [] = []
boardToCells b = let
    minX = fst $ minimum b
    maxX = fst $ maximum b
    minY = minimum $ map snd $ b
    maxY = maximum $ map snd $ b
    in [ [ if elem (x, y) b then 1 else 0 | x <- [minX..maxX] ] | y <- [minY..maxY] ]
