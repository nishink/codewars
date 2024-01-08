-- https://www.codewars.com/kata/5886e082a836a691340000c3
-- Simple Fun #27: Rectangle Rotation

module RectangleRot.JorgeVS.Kata where
rectangleRot :: Int -> Int -> Int
rectangleRot x y = let
    count x = floor $ (fromIntegral x / 2) / (sqrt 2 / 2)
    a = fromIntegral $ count x
    b = fromIntegral $ count y
    in ceiling (a * b / 2) * 4 + floor (a / 2) * 2 + floor (b / 2) * 2 + 1
 
{-
サンプルの図を眺めていると、点の数は3x5+2x4で求められることに気づく。
3x5が外側の緑の点、2x4が内側の緑の点の数。
ここから、長方形の辺の長さによって、緑の点が１辺あたり何個になるかが割り出せる。

ルート２の半分を１単位として、
長方形の辺の長さがいくつだったら、何単位分の点が入るかをカウントする。
全体をカウントすると大変なので、長方形を４分割した範囲で幾つになるかをカウントする。
->count関数

カウントがわかったら、店の数を計算する。
ここで、原点を通る４５度の直線上にある点とそうでない点は数え方が異なる。
直線上にない点の方は数を数えたら４倍すればいいが、
直線上にある点は４倍するとカウントが重複するので、
原点以外は２倍、原点は１でカウントする。

直線上にない点は、市松模様になるので、偶数の場合は単位の掛け算の半分。
奇数の場合は単位の掛け算の半分＋１が点の数になる。

直線上にある点も、２単位ごとに１個なので、２で割って切り捨てた値を２倍する。
-}