-- https://www.codewars.com/kata/58a6b5f28c08b1e9c40001e7
-- Challenge Fun #12: Visible Points

module VisiblePoints.Kata (visiblePoints) where

import Data.List

type Point = (Double,Double)

{-
visiblePoints :: [Point] -> Int
visiblePoints ps = maximum [ count p ps | p <- ps ]
  where
    count :: Point -> [Point] -> Int
    count a@(x,y) ps = let
        b = (x-y,x+y) -- 45 degrees
      in length [ p | p <- ps, 
                      tangent a <= tangent p,
                      tangent p <= tangent b,
                      quadrant a == quadrant p ||
                      quadrant b == quadrant p ||
                      quadrant (add (quadrant a) (quadrant b)) == quadrant p ]
-}

visiblePoints :: [Point] -> Int
visiblePoints ps = let
    cps = calcPoints ps
  in maximum [ countP p cps | p <- cps ]

calcPoints ps = [ 
    ( tangent a, tangent b, quadrant a, quadrant b, 
      quadrant (add (quadrant a) (quadrant b)) ) | a@(x,y) <- ps, let b = (x-y,x+y) ]

countP (ta,tb,qa,qb,qx) ps =
    length [ tp | (tp,_,qp,_,_) <- ps, ta <= tp, tp <= tb, qa == qp || qb == qp || qx == qp ]

tangent :: Point -> Double
tangent (x,y) = y / x

quadrant :: Point -> Point
quadrant (x,y) = (signum x, signum y)

add :: Point -> Point -> Point
add (aX,aY) (bX,bY) = (aX+bX,aY+bY)

{-
点A(x,y)の45度回転した位置は、点B(x-y,x+y)で表せる。
例、(3,2)->(1,5), (-1,2)->(-3,1), (0,2)->(-2,2)
あとは、その間にあるというのをどう数えればいいか。

全部が右上（第一象限）にある場合は、Ay/Ax <= y/x <= By/Bx にあればよい。
問題は、他のエリアに点がある場合や、垂直や水平をまたぐ場合だ。

他のエリアの場合も計算してみると、上記の比較で大丈夫そうである。
ではどこで上下が逆転するかというと、x=0のところだ。
y/0 = ∞ のため、x=0のときだけ特別な扱いが必要と思われる。

またy=0を超えると同じ値が出てくるので、こちらの方が厄介かも知れない。
xの値が正か負かで条件を分けることになるだろう。
もしくは、点AまたはBと同じ象限に点があるかどうかで判定するか。
象限は軸を含まないので、軸上にある点をどう扱うか。

ある点が、点AからBの間にあるかどうかをどうやって判定するか。
点を１つ取り出し、その点の45度回転した先の点を割り出す。


-}

-- 45度（π/4ラジアン）
d45 = pi / 4

-- 360度（2πラジアン）
d360 = pi * 2

-- 点の集合をラジアン角度に変換する
-- その際、0度近辺にあるものは+360度して二重化する
angles :: [Point] -> [Double]
angles ps = [ atan2 y x | (x, y) <- ps ] ++ 
            [ atan2 y x + d360 | (x, y) <- ps, atan2 y x < d45 - pi ]

-- 角度に変換したものをソートする

-- ソートしたものを最初から順に見ていく
-- ある角度から45度の範囲内にある角度の数を数え、記録する。
-- 次の角度についても同様。
-- 前の角度より、次の角度の方が数が多ければ記録を更新する。
-- 最後まで確認したとき、最大の数が残る。
visiblePoints' :: [Point] -> Int
visiblePoints' ps = let
    as = sort $ angles ps
  in maximum [ length $ filter (\x -> a <= x && x <= a + d45 + 0.00000000001) as | a <- as ]

vp :: [Double] -> [Int]
vp [] = []
vp (a:as) = (1 + length (takeWhile (<= a + d45 + 0.00000000001) as)) : vp as

visiblePoints'' :: [Point] -> Int
visiblePoints'' ps = let
    as = sort $ angles ps
  in maximum $ vp as

{-
参考文献
https://www.careercup.com/question?id=5699136226590720
https://stackoverflow.com/questions/59955013/explanation-and-complexity-for-visible-points-in-codefight
-}
