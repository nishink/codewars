{-# Language RankNTypes #-}

-- https://www.codewars.com/kata/5ac739ed3fdf73d3f0000048
-- Church Booleans

module Church (not,and,or,xor) where

import Prelude hiding (Bool,False,True,not,and,or,(&&),(||),(==),(/=))
-- import Church.Preloaded (Boolean,false,true)

type Boolean = forall a. a -> a -> a -- this requires RankNTypes

false,true :: Boolean
false = \ t f -> f
true  = \ t f -> t


not :: Boolean -> Boolean
and,or,xor :: Boolean -> Boolean -> Boolean

not = \ a -> \ b c -> a c b
and = \ a b -> a b false
or  = \ a b -> a true b
xor = \ a b -> a (not b) b

{-
false = \ t f -> f
true  = \ t f -> t

true, falseの定義がこうなっている（\とtの間が空いているが、ラムダ式であることには変わりないようだ）ので、
false 1 2 => 2
true  1 2 => 1
と言う結果を返す。

notのみで数値を扱うことはできないので、
(not false) 1 2 => 1
(not true)  1 2 => 2
となるような定義を考える。
ただし、==が使えないのでif文が使えない。

どうしたものかを悩んでいたが、色々検索していたところ答えを見つけてしまった。
https://github.com/Risto-Stevcev/haskell-church-encodings/blob/master/RankNTypes/Church.hs

カンニングしてしまったので、せめて理屈を理解したい。

notについてはシンプルで、notの次に指定したものに対して逆の結果になるように定義している。
andを分解すると、\a b false -> \a -> \b -> \t f -> f
a = true, b = trueの場合、aはbを選択し、bはtを選択するのでtrueとなる。
a = true, b = falseの場合、aはbを選択し、bはfを選択するのでfalseとなる。
a = false, b = trueの場合、aはfalseを選択するのでfalseとなる。

orを分解すると、\a true b -> \a -> (\t f -> t) -> \b 
a = true, b = trueの場合、aはtrueを選択するのでtrueとなる。
a = falseの場合、aはbを選択するのでbがtrueならtrue、bがfalseならfalseである。

xorを分解すると、\a -> (\b c d -> \b d c) -> \b
a = trueの場合、aはnot bを選択するので、bがtrueならfalse、bがfalseならtrueとなる。
a = falseの場合、aはbを選択するので、bがtrueならtrue、bがfalseならfalseとなる。

-}