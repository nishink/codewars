-- https://www.codewars.com/kata/54c9fcad28ec4c6e680011aa
-- Merged String Checker

module Codewars.Exercise.MergeChecker where

isMerge :: String -> String -> String -> Bool
isMerge "" "" "" = True
isMerge "" _  _  = False -- パーツが余る
isMerge _  "" "" = False -- パーツが足りない
isMerge (x:xs) part1 part2 = let
    match x part = if null part then False else if x /= head part then False else True
    next =
        ( if match x part1 then [ isMerge xs (tail part1) part2 ] else [] ) ++
        ( if match x part2 then [ isMerge xs part1 (tail part2) ] else [] )
    in if null next then False else or next
{-
    s:  c o d e w a r s   = codewars
part1:  c   d   w         = cdw
part2:    o   e   a r s   = oears

・sの先頭の文字を取り出す。
・その文字がpart1またはpart2のいずれの先頭にもない場合はFalseとなり終了。
・part1かpart2のどちらかにある場合は、part1/2の先頭からその文字を除外し、以後繰り返す。
・sが最後の文字まで到達できたらTrueとなり終了。
・part1、part2の両方に同じ文字がある場合、両方のケースについて見ていく必要がある。
・part1、part2の文字が先に尽きることがあるので、
　同じ文字があるか以前に文字があるかを判定する必要がある。
-}