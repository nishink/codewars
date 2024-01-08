-- https://www.codewars.com/kata/5a28cf591f7f7019a80000de
-- Simple number sequence

module SimpleNumberSequence.Kata (missing) where

import Debug.Trace

missing :: String -> Maybe Int
missing ns = f 1
    where
        f fig = let
            result = search ns fig Nothing
            in
                if result /= Nothing then result
                else if length ns `div` 2 >= fig then f (fig + 1)
                else Nothing

search :: String -> Int -> Maybe Int -> Maybe Int
search "" _ miss = miss
search ns fig miss = let
    (n, remain) = splitAt fig ns
    n' = show $ read n + 1
    n'' = show $ read n + 2
    m' = take (length n') remain
    m'' = take (length n'') remain
    in
        if n' == m' then -- 連続している
            if m' == remain then miss -- 最後の数なら欠けていると判定したものを返す
            else search remain (length n') miss -- 残りも連続しているか
        else if n'' == m'' then -- 欠けた数を見つけた
            if miss == Nothing then -- 初めて見つけた
                if m'' == remain then Just (read n') -- 最後の数なら今見つけた物を返す
                else search remain (length n'') (Just (read n')) -- 残りも連続しているか
            else Nothing -- ２つ以上欠けている
        else Nothing -- 連続していない

{-
数字の列から欠けている数字を見つける。

missing("123567") = 4 
missing("899091939495") = 92
missing("9899101102") = 100
missing("599600601602") = -1 -- no number missing
missing("8990919395") = -1 -- error in sequence. Both 92 and 94 missing.

さて、欠けているというのは何をすればわかるのか。

少なくとも、２つ以上の数字がないと、間の数字が欠けているとは判断できない。
なので、数字列の長さの半分を超えたら、それはもう欠けているか判断できないエラーということだ。

手順としては次のとおりになる。
・最初の一文字を取って数字にする。
・その数字を＋１する。
・＋１した数字と同じ桁数を、残りの文字から取得する。
・＋１した数字と、残りから取得した数字が一致したら、それは連続した数字であるため、
　残りの数字列に対して同じことを繰り返す。
・＋１した数字と、残りから取得した数字が一致しない場合、
　さらに＋１した数字と一致するかを判定する。
・さらに＋１した数字と一致したら、欠けている数字が判明する。
　一致しない場合は、連続していないと判断する。
・欠けている数字が見つからなかった場合、
　最初に取得する文字数を＋１する。
　ただし、全体の長さの半分を超える場合はエラーとする。

-}