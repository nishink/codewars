-- https://www.codewars.com/kata/5aa67541373c2e69a20000c9
-- Tic-Tac-Toe (3D)

module TicTacToe3D (play) where

play :: [(Int,Int,Int)] -> String
play moves = judge moves 1 [] []

judge [] _ _ _ = "No winner"
judge (m:ms) n os xs
    | odd n  && check os' = "O wins after " ++ show n ++ " moves"
    | even n && check xs' = "X wins after " ++ show n ++ " moves"
    | otherwise = judge ms (n+1) os' xs'
    where
        os' = if odd n then (m:os) else os
        xs' = if even n then (m:xs) else xs

check moves = or [
    or [ same [(0,y,z),(1,y,z),(2,y,z),(3,y,z)] | y<-[0..3], z<-[0..3] ],
    or [ same [(x,0,z),(x,1,z),(x,2,z),(x,3,z)] | x<-[0..3], z<-[0..3] ],
    or [ same [(x,y,0),(x,y,1),(x,y,2),(x,y,3)] | x<-[0..3], y<-[0..3] ],
    or [ same [(0,0,z),(1,1,z),(2,2,z),(3,3,z)] | z<-[0..3] ],
    or [ same [(0,y,0),(1,y,1),(2,y,2),(3,y,3)] | y<-[0..3] ],
    or [ same [(x,0,0),(x,1,1),(x,2,2),(x,3,3)] | x<-[0..3] ],
    or [ same [(0,3,z),(1,2,z),(2,1,z),(3,0,z)] | z<-[0..3] ],
    or [ same [(0,y,3),(1,y,2),(2,y,1),(3,y,0)] | y<-[0..3] ],
    or [ same [(x,0,3),(x,1,2),(x,2,1),(x,3,0)] | x<-[0..3] ],
    same [(0,0,0),(1,1,1),(2,2,2),(3,3,3)],
    same [(0,0,3),(1,1,2),(2,2,1),(3,3,0)],
    same [(0,3,0),(1,2,1),(2,1,2),(3,0,3)],
    same [(0,3,3),(1,2,2),(2,1,1),(3,0,0)]
    ]
    where
        same ms = and [ elem m moves | m<-ms ]

moves :: [(Int,Int,Int)]
moves = [ (0,2,1)
           , (0,2,2)
           , (1,2,1)
           , (1,2,2)
           , (2,2,1)
           , (2,2,2)
           , (3,2,1)
           ]

{-
3DのOXゲーム。
４目並べである。

先手はO、後手はXで固定。
(x,y,z)の手番が提示されるので、何手目でどちらが勝ったか、
あるいは勝敗が決まってないかを判定する。
手番は、勝敗が決まった後も提示されるので、
毎回勝敗が決まっているかを判定しないといけない。

勝敗は、同じ記号が４つ並んだら決まる。
縦横斜めの他に、立体の対角線も並んだという判定になる。
したがって、勝敗判定となるのは以下のパターン。

・縦横高さ
(0-3,y,z)
(x,0-3,z)
(x,y,0-3)
・斜め
(0-3,0-3,z)
(0-3,y,0-3)
(x,0-3,0-3)
・逆斜め
(0-3,3-0,z)
(0-3,y,3-0)
(x,0-3,3-0)
・立体の対角線
(0-3,0-3,0-3)
(0-3,0-3,3-0)
(0-3,3-0,0-3)
(0-3,3-0,3-0)


-}