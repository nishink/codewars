-- https://www.codewars.com/kata/5544c7a5cb454edb3c000047
-- Bouncing Balls

module Codewars.Kata.BouncingBall where

bouncingBall :: Double -> Double -> Double -> Integer
bouncingBall h bounce window = 
    if h <= 0 || bounce <= 0 || bounce >= 1 || window >= h
        then -1
        else go h 0
    where
        go height count
            | height > window = go (height * bounce) (count + if count == 0 then 1 else 2)
            | otherwise = count

{-
説明：

高いビルのn階で子供がボールで遊んでいる。このフロアの地上の高さ、hは知られています。

彼はボールを窓から落とした。ボールは（例えば）高さの3分の2まで跳ね返る（跳ね返る0.66）。

彼の母親は地面から1.5メートルの窓から外を見ている。

母親は、ボールが窓の前で通過するのを何回見ますか（ボールが落ちて跳ねているときも含めて）？

有効な実験を行うには、次の3つの条件を満たす必要があります。

フロートパラメータ「h」（メートル）は0より大きくなければなりません
フロートパラメータ「バウンス」は0より大きく、1より小さい必要があります
フロートパラメータ「ウィンドウ」はh未満でなければなりません。
上記の3つの条件がすべて満たされた場合、正の整数を返します。そうでない場合は-1を返します。

注意：

ボールは、リバウンドボールの高さがウィンドウパラメータより厳密に大きい場合にのみ見ることができます。

例：

- h = 3, bounce = 0.66, window = 1.5, result is 3

- h = 3, bounce = 1, window = 1.5, result is -1 

(Condition 2) not fulfilled).
-}
