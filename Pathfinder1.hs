-- https://www.codewars.com/kata/5765870e190b1472ec0022a2
-- Path Finder #1: can you reach the exit?

module Pathfinder1 where

import Debug.Trace

import Data.Array.Unboxed (UArray)
import Data.Array.IArray
import qualified Data.Set as S
import Data.List (lines, intercalate, intersect, (\\), nub)

pathFinder :: String -> Bool
pathFinder maze = find ([start], (filter (/=start) xs))
    where
        xs = mazeToList maze
        start = (0,0)
        goal = maximum xs
        find (searched, unsearched) = 
            if elem goal searched then True 
            else if searched == [] then False 
            else find (findPath searched unsearched)

type Pos = (Int,Int)
type Path = [Pos]

-- '.'の位置を(x,y)に変換
mazeToList :: String -> Path
mazeToList maze = let
    xs = lines maze
    in concat [ 
        [ (snd col, snd row) | col <- zip (fst row) [0..], fst col == '.' ] 
        | row <- zip xs [0..] ]

-- 上下左右の隣の位置
neighbour :: Pos -> Path
neighbour (x,y) = [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

findPath :: Path -> Path -> (Path,Path)
findPath path unsearchedPath = let
    target = nub $ concatMap neighbour path
    searched = intersect unsearchedPath target
    unsearched = unsearchedPath \\ target
    in (searched, unsearched)

    

{-
"."の位置を(x,y)に変換し、移動可能な場所のリストを用意する。
スタートから上下左右に移動できるか探索。
移動できる箇所がある場合は、それを次の候補とし、スタートは探索済みに移す。
次の候補からそれぞれ上下左右に移動できるか探索。（このとき探索済みは対象外）
移動先がなくなった、もしくはゴールまで辿り着いたら終了。
-}

solveMaze :: [String] -> Bool
solveMaze xs = dropWhile ((/= goal) . snd) queue /= []
    where
        height = length xs
        width = length $ head xs
        start = (1, 1)
        goal = (width, height)
        maze' = listArray (start, goal) (concat xs) :: UArray (Int, Int) Char
        maze = trace ("maze:" ++ (show maze')) maze'
        queue = (0, start) : go (S.singleton start) queue
        go s ((dist, (i, j)) : qs) = next' ++ go s' qs
            where
                next = filter f2 $ filter f1 [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
                next' = [(dist + 1, p) | p <- next]
                s' = foldr S.insert s next
                f1 pos
                    | inRange (start, goal) pos, (maze ! pos) == '.' = True
                    | otherwise = False
                f2 = (`S.notMember` s)

maze = intercalate "\n"
        [ ".W..."
        , ".W..."
        , ".W.W."
        , "...W."
        , "...W."
        ]


-------------------------------------------------------------
-- https://zenn.dev/forest1040/articles/1f67647c12cd12d4c524
-- このアルゴリズムそのままだと、ゴールに辿り着かない場合に計算が終わらない。
solve :: Int -> Int -> [String] -> Int
-- dropWhileでqueueからgoalが出てくるまで取り出す
solve n m xss = fst . head $ dropWhile ((/= goal) . snd) queue
  where
    -- startの位置 (x,y)=(2,1) ※1始まり fstに位置、sndにcharデータが入る
    (start, _) = head . filter ((== 'S') . snd)
               $ zip [(i, j) | i <- [1 .. n], j <- [1 .. m]] (concat xss)
    -- goalの位置 (x,y)=(9,10) ※1始まり
    (goal, _)  = head . filter ((== 'G') . snd)
               $ zip [(i, j) | i <- [1 .. n], j <- [1 .. m]] (concat xss)
    -- 地図データ
    maze = listArray ((1, 1), (n, m)) (concat xss) :: UArray (Int, Int) Char
    -- リスト(queueで余再帰) 「:」でつなげていく
    -- queueの中身(手数, 位置)
    -- 開始位置をData.Setに変換(S.singleton start)して、goにわたす
    queue = (0, start) : go (S.singleton start) queue
    -- 探索 sは、既に探索済みの位置情報（Data.set型）
    go s ((dist, (i, j)):qs) = next' ++ go s' qs
      where
        -- 4方向を探索する
        next  = filter f2 $ filter f1 [(i - 1, j), (i + 1, j), (i, j - 1), (i, j + 1)]
        -- 移動できる位置集合と手数(dist)を返す
        next' = [(dist + 1, p) | p <- next]
        -- 探索の開始位置をData.Setに放り込む
        s' = foldr S.insert s next
        -- その方向に移動可能か
        f1 pos
            | inRange ((1, 1), (n, m)) pos
                , (maze ! pos) `elem` ['.', 'G'] = True
            | otherwise                          = False
        -- 既にその場所を訪れたか
        f2 = (`S.notMember` s)

