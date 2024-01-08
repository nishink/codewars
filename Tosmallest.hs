-- https://www.codewars.com/kata/573992c724fc289553000e95
-- Find the smallest

module Codewars.G964.Tosmallest where

smallest :: Integer -> (Integer, Int, Int)
smallest n = let
    s = show n
    l = length s
    in minimum [ (read (insert y c (delete x s)) :: Integer, x, y) | x <- [0..l-1], y <- [0..l-1], let c = s !! x ]

insert :: Int -> Char -> String -> String
insert n c s = let
    (h,t) = splitAt n s
    in h ++ [c] ++ t

delete :: Int -> String -> String
delete n s = let
    (h,t) = splitAt n s
    in h ++ tail t

{-
さて、一見しただけでは問題の意味がわからない。
提示されている条件は以下だ。

1. the smallest number you got
2. the index i of the digit d you took, i as small as possible
3. the index j (as small as possible) where you insert this digit d to have the smallest number.

1. 結果が最も小さい数になる、というのはわかる。
2. 数字dのインデックスiをとる、iはできるだけ小さい。
　　は、同じ数字が複数ある場合はインデックスの小さい方を取れということだろう。
3. できるだけ小さいインデックスjにこのjを挿入して、最も小さい値を得る。

よくわからないのでサンプル問題を見てみる。

dotest 261235 (126235, 2, 0)
　これは数字の１をインデックス２から取ってインデックス０に入れている。
dotest 209917 (29917, 0, 1)
　これは数字の２をインデックス０から取ってインデックス１に入れている。
dotest 285365 (238565, 3, 1)
　これは数字の３をインデックス３から取ってインデックス１に入れている。
dotest 269045 (26945, 3, 0)
　これは数字の０をインデックス３から取ってインデックス０に入れている。
dotest 296837 (239687, 4, 1)
　これは数字の３をインデックス４から取ってインデックス１に入れている。

なんとなく、意味はわかってきた。
インデックスiとjの組み合わせを全部試して、
結果のタプルをソートして最も小さいものが答えになりそうだ。



-}




