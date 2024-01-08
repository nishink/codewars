-- https://www.codewars.com/kata/54120de842dff35232000195
-- NIM the game

module NIM where

import Data.Bits
import Data.List

-- | Returns the index and the number picked from a board
chooseMove :: [Int] -> (Int,Int)
chooseMove piles = let
    n = nimSum piles
    idx = head $ findTargetPiles piles n
    straw = (piles !! idx)
    in (idx, straw - (straw `xor` n))

-- ニム和
nimSum :: [Int] -> Int
nimSum xs = foldl1 xor xs

-- 最上位ビットの桁
maxBit :: Int -> Int
maxBit n = f n 0
    where
        f 0 d = d
        f x d = f (shiftR x 1) (d + 1)

-- 最上位ビットのみを残した値
maxBitOnly :: Int -> Int
maxBitOnly n = let
    d = maxBit n
    in if d == 0 then 0 else shiftL 1 (d - 1)

-- strawをとるpileを探す
findTargetPiles :: [Int] -> Int -> [Int]
findTargetPiles piles n = let
    d = maxBitOnly n
    in findIndices (\x -> x .&. d /= 0) piles


{-
NIMの攻略法は、
各pileのstrawの排他的論理和が0になるようにとるらしい。

https://ja.wikipedia.org/wiki/ニム

どうやってとるかというと、

全てのpileの排他的論理和（ニム和という）を計算し、
その中で最上位にビットが立っている桁dを見つける。

pileの中から、桁dが立っているものkを探し、
そこからニム和のビットが反転する本数になるようにstrawをとる。
桁dが立っているかどうかはビット論理積(.&.)が０でないことで判定する。

ビットを反転するには、排他的論理和を使う。
すなわち、pile-kのstraw本数 `xor` NIM和　がとったあとのstrawの本数になる。 
そのため、何本取るかはpile-kのstraw本数から、上記xorの結果を引けば求められる。
-}