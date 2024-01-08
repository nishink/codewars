-- https://www.codewars.com/kata/5811aef3acdf4dab5e000251
-- Mixbonacci

module Example (mixbonacci) where

import Data.List (tails, transpose)

mixbonacci :: [String] -> Int -> [Integer]
mixbonacci pattern length' = let
    patterns = if null pattern then [] else take length' $ cycle pattern
    in bonacci patterns 0 0 0 0 0 0
    where
        bonacci [] _ _ _ _ _ _ = []
        bonacci (x:xs) fibn padn jacn peln trin tetn = case x of
            "fib" -> fib fibn : bonacci xs (fibn+1) padn jacn peln trin tetn
            "pad" -> pad padn : bonacci xs fibn (padn+1) jacn peln trin tetn
            "jac" -> jac jacn : bonacci xs fibn padn (jacn+1) peln trin tetn
            "pel" -> pel peln : bonacci xs fibn padn jacn (peln+1) trin tetn
            "tri" -> tri trin : bonacci xs fibn padn jacn peln (trin+1) tetn
            "tet" -> tet tetn : bonacci xs fibn padn jacn peln trin (tetn+1)

fib n = fibs !! n
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

pad n = pads !! n
pads = 1 : 0 : 0 : zipWith (+) pads (tail pads)

jac n = jacs !! n
jacs = 0 : 1 : zipWith (+) (map (2*) jacs) (tail jacs)

pel n = pels !! n
pels = 0 : 1 : zipWith (+) pels (map (2*) $ tail pels)

tri n = tris !! n
tris = 0 : 0 : 1 : zipWith (+) tris (tail (zipWith (+) tris $ tail tris))

tet n = tets !! n
tets = 0 : 0 : 0 : f [0,0,0,1] where
    f xs = y : f (y:xs) where
        y = sum $ head $ transpose $ take 4 $ tails xs

{-
６種類のフィボナッチを代表する数列を並べる問題。
まずそれぞれの数列について確認する。

・フィボナッチ
以下の定義が有名だが、処理速度はあまり良くない。
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
以下のほうがhaskell的には良いらしい。
fib n = fibs !! n
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

・パドバン
定義としては以下の通り。
pad 0 = 1
pad 1 = 0
pad 2 = 0
pad n = pad (n-2) + pad (n-3)
ただしhaskellでは以下の方が良い。
pad n = pads !! n
pads = 1 : 0 : 0 : zipWith (+) pads (tail pads)

・ヤコブスタール
jac 0 = 0
jac 1 = 1
jac n = jac (n-1) + 2 * jac (n-2)

jac n = jacs !! n
jacs = 0 : 1 : zipWith (+) (map (2*) jacs) (tail jacs)

・ペル
pel 0 = 0
pel 1 = 1
pel n = 2 * pel (n-1) + pel (n-2)

pel n = pels !! n
pels = 0 : 1 : zipWith (+) pels (map (2*) $ tail pels)

・トリボナッチ
tri 0 = 0
tri 1 = 0
tri 2 = 1
tri n = tri (n-1) + tri (n-2) + tri (n-3)

tri n = tris !! n
tris = 0 : 0 : 1 : zipWith (+) tris (tail (zipWith (+) tris $ tail tris))

・テトラナッチ
tet 0 = 0
tet 1 = 0
tet 2 = 0
tet 3 = 1
tet n = tet (n-1) + tet (n-2) + tet (n-3) + tet (n-4)

tet n = tets !! n
tets = 0 : 0 : 0 : f [0,0,0,1] where
    f xs = y : f (y:xs) where
        y = sum $ head $ transpose $ take 4 $ tails xs

さて数列の準備ができたところで肝心の問題を解く方法について考える。

まず、与えられるのはパターンと要素数n。
パターンは ['fib', 'fib', 'pel'] のように与えられるので、
このパターンが無限に続く中からn個を取り出す方法を考える。

patterns = take n $ cycle pattern

そして、patternsを最初から見ていって、パターンに当てはまる数列から１つずつ数を取り出す。
取り出した後は引数を１増やす、と言ったことを繰り返せばよい。

-}