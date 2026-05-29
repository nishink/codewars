-- https://www.codewars.com/kata/597ef546ee48603f7a000057
-- Most profit from stock quotes

module MostProfit (profit) where

profit :: [Int] -> Int
profit xs = sum [max 0 (m - p) | (p, m) <- zip xs futureMax]
  where
    futureMax = tail $ scanr1 max xs
{-
株価から最も利益を上げる。

株価は日付順に配列として保存されます。株式利益とは、株式の売買における価格差です。
毎日、株式を1単位購入するか、すでに購入した株式単位を任意に売却するか、あるいは何もしないことができます。
したがって、最も利益が高いのは、株価の系列におけるすべてのペアの最大の差です。

@param {array} quotes
@return {number} max profit
例：

 [ 1, 2, 3, 4, 5, 6 ]        => 15  (buy at 1,2,3,4,5 and then sell all at 6)
 [ 6, 5, 4, 3, 2, 1 ]        => 0   (nothing to buy for profit)
 [ 1, 6, 5, 10, 8, 7 ]       => 18  (buy at 1,6,5 and sell all at 10)
 [ 1, 2, 10, 3, 2, 7, 3, 2 ] => 26  (buy at 1,2 and sell them at 10. Then buy at 3,2 and sell them at 7)

アルゴリズムの要点
株価リストの各日について、その日の後で到達可能な最大価格を調べる
その最大価格より安ければ「買って後で売る」利益を加算
これをすべての期間について行うと総利益が得られる
-}