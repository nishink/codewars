-- https://www.codewars.com/kata/5672682212c8ecf83e000050
-- Twice linear

module Codewars.G964.DblLinear where 

import Data.Set

dblLinear :: Int -> Integer
dblLinear n = dllist !! n
dllist = f $ singleton 1 where
    f s = m : (f $ insert (3*m+1) $ insert (2*m+1) s') where
        (m, s') = deleteFindMin s

dblLinear' :: Int -> Integer
dblLinear' n = f n [1] 0 0
    where
        f 0 u _ _= last u
        f n u p2 p3 = let
            y = 2 * (u !! p2) + 1
            z = 3 * (u !! p3) + 1
            newU = min y z
            newP2 = if newU == y then p2+1 else p2
            newP3 = if newU == z then p3+1 else p3
            in f (n-1) (u ++ [newU]) newP2 newP3

{-
まともに考えても解けそうにないのでアルゴリズムを探した。

JavaScriptによる実装
https://github.com/dwqs/codewars-practices/blob/master/codewars/201808/twice-linear.md
Pythonによる実装
https://medium.com/@peteryun/algo-twice-linear-ba882c587a3e

ただ、この通り実装するとタイムアウトしてしまう。

より効率の良い方法を求めて探していくと、そのものズバリの実装を見つけた。

https://codereview.stackexchange.com/questions/195788/codewars-twice-linear-is-running-too-long
https://codereview.stackexchange.com/questions/147413/generating-double-linear-sequence-in-haskell
https://oeis.org/A002977

しかしまあ、これをコピペして「はいできました」では流石に何もやってないことになってしまう。
自分なりにコードを理解しよう。

import Data.Set
a002977 n = a002977_list !! (n-1)
a002977_list = f $ singleton 1 where
   f :: Set Integer -> [Integer]
   f s = m : (f $ insert (3*m+1) $ insert (2*m+1) s') where
        (m, s') = deleteFindMin s

まずData.Setは重複しないデータの組である。
singleton 1 で、まず要素が「1」のみのSetを作っている。
関数fはSetをListに変換しているようだが、
deleteFindMinはなんだろう。

https://hackage.haskell.org/package/containers-0.6.7/docs/Data-Set.html
hackageによると、deleteFindMinはSetの中で最小の値を削除しているように見える。
が、結果として返却しているのが(最小値,最小値を削除した後のSet)なので、
後で最小値をまた追加していることから結局削除はしていない。

insertは何をしているのか。
指定した値をSetに追加し、追加した後のSetを返している。
なので、ここでは最小値mに対する2m+1と3m+1をSetに追加している。

そのSetをまた関数fにかけるとどうなるのか。
まず、最初はf [1]なので、deleteFindMin sは(1,[])となり、
1 : f [3,4]となる。
次の回では、deleteFindMin sは(3,[4])となり、
3 : f [4,7,10]となる。
-}

