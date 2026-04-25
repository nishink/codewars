-- https://www.codewars.com/kata/5779b0f0ec883247b2000117/train/haskell
-- Peano numbers

module Haskell.Codewars.Peano where
import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)

data Peano = Zero | Succ Peano deriving (Eq, Show)

add, sub, mul, div :: Peano -> Peano -> Peano
-- Addition
add p1 Zero = p1
add p1 (Succ p2) = add (Succ p1) p2
-- Subtraction
sub p1        Zero      = p1
sub Zero      (Succ p2) = error "negative number"
sub (Succ p1) (Succ p2) = sub p1 p2
-- Multiplication
mul p1 Zero = Zero
mul p1 (Succ Zero) = p1
mul p1 (Succ p2) = add p1 (mul p1 p2)
-- Integer division
div p1 Zero = error "divide by 0"
div p1 (Succ Zero) = p1
div p1 p2 = f p1 Zero
    where
        f Zero acc = acc
        f p    acc = if compare p p2 /= LT then f (sub p p2) (Succ acc) else acc

even, odd :: Peano -> Bool
-- Even
even Zero        = True
even (Succ Zero) = False
even (Succ p)    = not (even p)
-- Odd
odd p = not (even p)

compare :: Peano -> Peano -> Ordering
-- Compare
compare Zero Zero = EQ
compare p1   Zero = GT
compare Zero p2   = LT
compare (Succ p1) (Succ p2) = compare p1 p2
{-
ペアノ数は、ゼロ値と後継関数のみを使用して自然数を表す簡単な方法です。
ペアノ数を次のように表すことができます。

data Peano = Zero | Succ Peano
ここでは、ZeroとSuccがコンストラクタです。

Zero :: Peano
Succ :: Peano -> Peano
どこで:

0によって表されるZero
1によって表されるSucc Zero
2によって表されるSucc (Succ Zero)
3によって表されるSucc (Succ (Succ Zero))
など。
このカタでのあなたの使命は、ピアノ番号で次の操作を実装することです。

add：追加
sub：減算(エラー:"negative number"
mul：掛け算
div：整数除算 (エラー: "divide by 0"
even：偶数
odd：奇数
compare：比較(LT、GTまたはEQ)

これらの関数およびnotのみを使用すること。
-}