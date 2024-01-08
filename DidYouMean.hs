-- https://www.codewars.com/kata/5259510fc76e59579e0009d4
-- Did you mean ...?

module DidYouMean (findMostSimilar) where

import Data.List

import Data.Array

lev''' :: (Eq a) => [a] -> [a] -> Int
lev''' xs ys = levMemo ! (n, m)
  where levMemo = array ((0,0),(n,m)) [((i,j),lev i j) | i <- [0..n], j <- [0..m]]
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        lev 0 v = v
        lev u 0 = u
        lev u v
          | xa ! u == ya ! v = levMemo ! (u-1, v-1)
          | otherwise        = 1 + minimum [levMemo ! (u, v-1),
                                            levMemo ! (u-1, v),
                                            levMemo ! (u-1, v-1)] 

findMostSimilar :: [String] -> String -> String
findMostSimilar dictionary term = let
    sims = map (similarity term) dictionary
    idx = head $ elemIndices (minimum sims) sims
  in dictionary !! idx

similarity :: String -> String -> Int
similarity term word = let
    (longer, shorter) = if length term > length word then (term, word) else (word, term)
    common = shorter \\ (shorter \\ longer)
    mat = countMatch longer $ padWord longer common
  in (length longer - mat)


countMatch :: String -> String -> Int
countMatch term word = sum [ 1 | i <- [0..length term-1], term !! i == word !! i ]
-- term & word are same length

padWords :: String -> String -> [String]
padWords longstr shortstr = let
    dif = length longstr - length shortstr
  in [ replicate n '_' ++ shortstr ++ replicate (dif-n) '_' | n <- [0..dif] ]

padWord :: String -> String -> String
padWord ""     _      = ""
padWord longer ""     = replicate (length longer) '_'
padWord longer (x:xs) = let
    l = length $ takeWhile (/=x) longer
  in if l == length longer
       then replicate l '_'
       else replicate l '_' ++ [x] ++ padWord (drop (l+1) longer) xs


{-
similarity :: String -> String -> Int
similarity term word = let
    (longer, shorter) = if length term > length word then (term, word) else (word, term)
    mat = maximum $ map (countMatch longer) $ padWords longer shorter
  in (length longer - mat)


similarity :: String -> String -> Int
similarity term word = let
    cmn = term \\ ( term \\ word ) -- common letters
    dis = sum [ abs (getIndex c term - getIndex c word) | c <- cmn ]
  in length word + length term - 2 * length cmn + dis
  where
    getIndex c s = head $ elemIndices c s
-}

{-

heaven

haskell
haskel
hasken
hesken
heaken
heaven

java
hava
heava
heavan
heaven

h_askell
hea_ven_
 -> replace 5 letters

j_ava_
heaven
 -> replace 4 letters

まず双方の文字で最初に一致するものを探す。
一致するものが見つかったとき、indexが一致しなければ
間に文字を詰めて一致させる。
そうすればreplaceする文字の数だけを考えれば良い。

heaven
heavenl
heavell
heakell
heskell
haskell

heaven
heave
heava
eava
java

問題文としては、提示された単語に近いものを辞書から探す、ということで、
提示された単語の方を徐々に変化させていくようにしないといけない。

文字が一致する、というのはどうやって機械的に判定すればいいだろうか。

heavenをもとに、一致する文字をhaskellから探すと、
hの次はeがヒットしてしまう。aのほうが近いのに。

intersectではどうか。

Prelude Data.List> intersect "heaven" "haskell"
"heae"
Prelude Data.List> intersect "haskell" "heaven"
"hae"
Prelude Data.List> intersect "haskell" $ intersect "heaven" "haskell"
"hae"
Prelude Data.List> intersect "heaven" "java"
"av"
Prelude Data.List> intersect "java" "heaven"
"ava"
Prelude Data.List> intersect "java" $ intersect "heaven" "java"
"ava"

うまいこととれてない。


Prelude Data.List> "heaven" \\ "haskell"
"ven"
Prelude Data.List> "heaven" \\ "java"
"heen"
Prelude Data.List> "heaven" \\ ("heaven" \\ "java")
"av"
Prelude Data.List> "heaven" \\ ("heaven" \\ "haskell")
"hae"

差集合なら、それっぽいのがとれた。

heavenをjavaに変換するには、四文字削除、二文字追加。
heavenをhaskellに変換するには、三文字削除、四文字追加。

replaceのことを考えてないので、合ってないかもしれない。

expected: "muj"
 but got: "uma"
(["strir","ltvufrhnowp","hmpqngxdntbj","uma","muj","gxdppld"],"aspj")

やっぱりあってなかった。

共通する文字がとれたら、その文字がどこにあるかも加味しないといけないようだ。

aspjとuma、共通はaだが、それぞれ一文字目と三文字目にある。この場合、二文字分ずらすという作業が入る。
aspjとmuj、共通はjだが、それぞれ四文字目と三文字目なので、ずらすのは一文字分だけである。

というアルゴリズムでやってみたが、かえって遠くなった気がする。

expected: "uvbkoc"
 but got: "nqa"
(["xbofctwe","sgrvalzxnjw","uvbkoc","nqa"],"tljgqbqoy")


やはり、文字を埋める方向で考えてみるか。
比較する文字の長さが違う場合、前後に文字を埋めることで一致する文字が多くならないかを確認したい。

tljgqbqoy
uvbkoc___ - no match
_uvbkoc__ - no match
__uvbkoc_ - no match
___uvbkoc - b and c match

もっとも一致の多いパターンがわかれば、あとは置き換えるだけ。

tljgqbqoy
xbofctwe_ - no match
_xbofctwe - no match

これだと、前後にしか_を入れてない。間に_が入るパターンが網羅できていない。

heaven
h___e__
haskell

heaven
__av__
java

expected: "lu"
 but got: "dqieby"
(["uewvhos","gqbvxsiskg","rqugqmzmunrp","dqieby","ynnpkmwwbi","lu"],"zlkrueza")

12345678
zlkrueza
__dqieby
dqieby

zlkrueza
_l__u___
lu

expected: "mtyrkosw"
 but got: "xthvkrelr"
(["b","xthvkrelr","mtyrkosw"],"veysswpkgvvl")

veysswpkgvvl
mtyr___kosw_
mty_sw______
mtyrDDsw
mtyrkosw

veysswpkgvvl
___xthvkrelr

やはり、_は前後だけではなく間に入るパターンも見ないといけないようだ。

うまいこと、間に_を入れるにはどうしたらよいか。

zlkrueza
lu______
l_u_____
l__u____
l___u___
l____u__
l_____u_
l______u
_lu_____
 :
_l__u___
lu

パターンを全部試すのだと、組み合わせが爆発しそう。

*DidYouMean> "mtyrkosw" \\ ("mtyrkosw" \\ "veysswpkgvvl")
"yksw"

短い方から長い方の差集合をとり、それを短い方からさらに差をとると、
共通する文字が短い方の順で得られる。
これを先頭から順に、長い方と一致する文字の位置まで埋めるというのはどうか。
一文字一致するごとに、一致したところまでの文字は捨てて、残りの文字についてまた一致する位置まで埋める。
長い方の単語の長さを超えた分は捨てる。

veysswpkgvvl
__y____k__sw
yksw

padWord :: String -> String -> String
padWord ""     _      = ""
padWord longer ""     = replicate (length longer) '_'
padWord longer (x:xs) = let
    l = length $ takeWhile (/=x) longer
  in replicate l '_' ++ [x] ++ padWord (drop (l+1) longer) xs

これでもダメだった。
下記のパターンに対応できない。

veysswpkgvvl
___xthvkrelr

やはり、組み合わせが爆発するのを覚悟で全てのパターンを試すか？
いや、その前に、前後に詰めた場合で一番ヒットしたものについて、
ヒットした文字を削って詰めて行くのはどうだろう。

veysswpkgvvl
___xthvkrelr -hit k
__xthvkrelr_
_xthvkrelr__
xthvkrelr___

veysswp*gvvl
___xthv_relr
__xthv_relr_
_xthv_relr__
xthv_relr___

veysswpkgvvl
____mtyrkosw
___mtyrkosw_ - hit k
__mtyrkosw__
_mtyrkosw___
mtyrkosw____ - hit y

veysswp*gvvl - del k
____mty_rosw
___mty_rosw_
__mty_rosw__
_mty_rosw___
mty_rosw____ - hit y

ve*sswpkgvvl - del y
mt_rkosw____
_mt_rkosw___
__mt_rkosw__
___mt_rkosw_ - hit k
____mt_rkosw

ve*sswp*gvvl - del y k
mt_r_osw____
_mt_r_osw___
__mt_r_osw__
___mt_r_osw_
____mt_r_osw

最初の時点で、kとyが候補になることがわかるので、そこから答えが導き出せないだろうか。
他の例で考えてみる。

heaven
haskell
heaven_ - hit h e
_heaven

heaven
java__
_java_ - hit a v
__java

zlkrueza
_lu_____ - hit l
___lu___ - hit u
lu

javascript
java______ - hit java
_java_____
__java____ - hit a
___java___
____java__
_____java_
______java

jav*script - del a
j_va______ - hit j v
_j_va_____
__j_va____
___j_va___
____j_va__
_____j_va_
______j_va

*a**script - del j v
___a______
____a_____
_____a____
______a___
_______a__
________a_
_________a

こういうパターンもあるので、最初の段階だけではやっぱりわからない。
一致した文字を削って繰り返す方法は有効かもしれない。

javascript
jaav

ただし、単純に削ってしまうと上記のパターンで先頭四文字が全てヒットしてしまうので、
削った文字は一致しない文字に置き換える必要がある。

----
答え合わせ。
レーベンシュタイン距離というアルゴリズムで判定できるらしい。

なんだー。もうあるんじゃん。

しかし、シンプルに実装しようとすると遅いらしい。

-}
lev :: (Eq a) => [a] -> [a] -> Int
lev [] [] = 0
lev [] ys = length ys
lev xs [] = length xs
lev (x:xs) (y:ys)
  | x == y    = lev xs ys
  | otherwise = 1 + minimum [lev xs (y:ys),
                             lev (x:xs) ys,
                             lev xs ys]


