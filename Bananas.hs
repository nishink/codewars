-- https://www.codewars.com/kata/5917fbed9f4056205a00001e
-- Bananas

module Bananas (bananas) where

bananas :: String -> [String]
bananas xs = crossout xs "banana"

crossout :: String -> String -> [String]
crossout "" "" = [""]
crossout "" ys = []
crossout xs "" = [replicate (length xs) '-']
crossout (x:xs) (y:ys)
    | x == y    = map (x:) (crossout xs ys) ++ map ('-':) (crossout xs (y:ys))
    | otherwise = map ('-':) (crossout xs (y:ys))

{-
入力した文字列に対して、"banana"と一致するかどうかを一文字ずつ見ていく。
入力が"bbananana"の場合、最初に"b"がヒットするので、
最初の文字が"b"のままで良い場合と、"-"に置き換えた場合に分岐する。

最初の文字ば"b"のままで良い場合、次は"a"と一致するかどうかをみる。
次の文字は"b"なので一致しない。なので"-"一択になる。

最初の文字を"-"に置き換えた場合、次も"b"と一致するかどうかをみる。
次の文字は"b"なので一致する。なので、また"b"のままで良い場合と"-"に置き換えた場合に分岐する。

これを繰り返し、"banana"最後まで辿り着いたものだけを残す。
-}