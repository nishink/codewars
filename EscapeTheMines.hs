-- https://www.codewars.com/kata/5326ef17b7320ee2e00001df
-- Escape the Mines ! (retired)

module EscapeTheMines where

import Data.List

type XY = (Int,Int)
data Move = U | D | R | L deriving (Eq, Show)

solve :: [[Bool]] -> XY -> XY -> [Move] 
solve grid miner exit = find ([miner], (filter (/=miner) xs))
    where
        xs = gridToList grid
        find (passed, unpassed) =
            if exit == head passed then routeToMove passed
            else let
                route = findRoute passed unpassed
                in  if passed == fst route then find (tail passed, unpassed)
                    else find route

-- 通れる場所を(x,y)に変換
gridToList :: [[Bool]] -> [XY]
gridToList grid =
    concat [ [ (snd row, snd col) | col <- zip (fst row) [0..], fst col ] | row <- zip grid [0..] ]

-- 上下左右の隣の座標
neighbour :: XY -> [XY]
neighbour (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

findRoute :: [XY] -> [XY] -> ([XY],[XY])
findRoute passedPos@(x:_) unpassedPos = let
    ns = intersect unpassedPos (neighbour x)
    in if ns /= [] 
        then (head ns : passedPos, filter (/= head ns) unpassedPos)
        else (passedPos, unpassedPos)

routeToMove :: [XY] -> [Move]
routeToMove [] = []
routeToMove [x] = []
routeToMove (x:xs) = let
    y = head xs
    z = (fst x - fst y, snd x - snd y)
    m = case z of
        (1,0) -> R
        (-1,0) -> L
        (0,1) -> D
        (0,-1) -> U
    in routeToMove xs ++ [m]
{-
通ったところをスタックに入れる。
通ったところがゴールなら終了。
そうでなければ、隣接している場所でまだ通ってないところを探す。
まだ通ってないところがあれば、そのうちの１つを次に通るところにする。
まだ通ってないところがないなら、スタックから１つ取り出して１つ戻る。
スタックから取り出すものがない場合、ゴールに辿り着けないと見なして終了。
-}

grid = [[True, False],
        [True, True]]

