-- https://www.codewars.com/kata/58b38256e51f1c2af0000081
-- Simple Fun #166: Best Match

module BestMatch (bestMatch) where

import Data.List

bestMatch :: [Int] -> [Int] -> Int
bestMatch ag zg = snd $ head $ sortMatch $ zip (zip ag zg) [0..]

sortMatch :: [((Int,Int),Int)] -> [((Int,Int),Int)]
sortMatch = sortBy (\x y -> let
    x1 = usf x
    y1 = usf y
    in if x1 == y1 then if fst x == fst y then compare x y else compare y x else compare y1 x1
    )

usf :: ((Int,Int),Int) -> Int
usf x = uncurry subtract (fst x)

{-
「AL-AHLY」と「Zamalek」はエジプトで最高のチームですが、「AL-AHLY」は常に彼らの間の試合に勝ちます。「ザマレク」の監督は、これまでで最高の試合を知りたい。
最高の試合は、彼らが最小のゴール差で負けた試合です。同じ違いを持つ複数の試合がある場合は、より多くのゴールを決めた「ザマレク」を選択してください。
プレイしたすべての試合に関する情報を考えると、ベストマッチのindex0-based）を返します。複数の有効な結果がある場合は、最小のインデックスを返します。

「AL-AHLY」(以下A)と「Zamalek」(以下Z)の、試合ごとの各ゴール数が与えられるので、
zipでタプルにし、さらに[0..]とタプルにする。
[((Aのゴール数,Zのゴール数),index),..]というリストができるので、
与えられた条件でソートする関数sortMatchを使ってソートし、最初に現れたもののインデックスが答え。

sortMatchは、タプルのリストを以下の条件でソートする。
・ゴール数の差が等しい場合はゴール数で比較し、ゴール数も一致する場合はインデックスの大小で比較する。
・ゴール数の差が等しいがゴール数が一致しない場合は、ゴール数が多いものが優先となるよう比較する。
・ゴール数の差が等しくない場合はゴール数の差が小さい方が優先となるよう比較する。

usfは、タプルからAチームとZチームのゴール数の差を求める。

-}