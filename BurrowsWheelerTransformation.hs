-- https://www.codewars.com/kata/54ce4c6804fcc440a1000ecb
-- Burrows-Wheeler-Transformation

module BWT where

import Data.List

-- | Encode an input sequence with the Burrows-Wheeler-Transformation.
encode :: Ord a => [a] -> ([a], Int)
encode xs = let
    sorted = sort [ t ++ h | n <- [0..length xs-1], let (h,t) = splitAt n xs ]
    in (map last sorted, head $ elemIndices xs sorted)


-- | Get back the input from a Burrows-Wheeler-Transformation.
decode :: Ord a => [a] -> Int -> [a]
decode xs idx = let
    pairs = [ ([c],i) | i <- [0..length xs-1], let c = (sort xs) !! i ]
    sorted = sort [ (c:fst p, snd p) | i <- [0..length xs-1], let p = pairs !! i, let c = xs !! i ]
    idxs = getIndices (zip (map snd pairs) (map snd sorted)) idx (length xs)
    in [ xs !! i | i <- idxs ]

getIndices :: [(Int,Int)] -> Int -> Int -> [Int]
getIndices _ _ 0 = []
getIndices ts i n = let
    ix = snd (ts !! i)
    in ix : getIndices ts ix (n-1)

{-

encodeは問題文にあるとおり簡単で、
元の文字列を一文字ずつ巡回シフトさせたものをソートし、（sorted）
各文字列の最後の文字と、元の文字列が何番目にあるかを取り出せば良い。

decodeのやり方は下記に載っている。
https://ja.wikipedia.org/wiki/ブロックソート

エンコードしたものを「BWT系列」と呼んでいるが、
BWT系列をソートすると、復号後の先頭文字が得られる。
まずはこの先頭文字とインデックスのペアを作成する。

次に、BWT系列は復号後の末尾文字である。
これを先頭文字とインデックスのペアの先頭に付与し、ソートする。

ソート前とソート後のインデックスを比較する。

-}

