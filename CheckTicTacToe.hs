-- https://www.codewars.com/kata/5f78635e51f6bc003362c7d9
-- Check that the situation is correct

module CheckTicTacToe (isItPossible) where

isItPossible :: String -> Bool
isItPossible s = let
    cX = count 'X' s
    c0 = count '0' s
    cm = checkmate s
    in (cX == c0 || cX == c0 + 1) && 
       (elem cm [0,1,2]) &&
       not (isWin 'X' s && isWin '0' s) &&
       if isWin 'X' s then cX == c0 + 1
       else if isWin '0' s then cX == c0
       else True

count c s = length $ filter (c==) s

checkmate s = sum [ 1 | l <- checkLines s, elem l ["XXX","000"] ]
isWin c s = elem [c,c,c] $ checkLines s

line s ids = map (s !!) ids
checkLines s = [
        line s [0,1,2],
        line s [3,4,5],
        line s [6,7,8],
        line s [0,3,6],
        line s [1,4,7],
        line s [2,5,8],
        line s [0,4,8],
        line s [2,4,6]
        ]

{-
公正なルールに則っているかどうかを以下で判断する。
・Xの数は0の数と等しいか、１つ多い。
・Xまたは0が縦横斜めに並んでいるケースが0か1か2である。
・Xが勝利している場合、Xの数は0の数+1である。
・0が勝利している場合、0の数はXの数と等しい。
・Xと0が両方勝利していることはない
-}