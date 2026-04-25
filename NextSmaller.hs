-- https://www.codewars.com/kata/5659c6d896bc135c4c00021e
-- Next smaller number with the same digits

module NextSmaller where 

import Data.List

nextSmaller :: Integer -> Maybe Integer
nextSmaller n
    | n == (read $ sort $ show n) = Nothing
    | otherwise = let
        val = reverse $ prev (reverse $ show n) []
        in if head val == '0' then Nothing else Just (read val)

prev [] acc = acc
prev [x] acc = x:acc
prev (x:y:xs) acc
    | x < y = let
        m = maximum $ filter (y>) (x:acc)
        r = sort $ y : delete m (x:acc)
        in r ++ m : xs
    | otherwise = prev (y:xs) (x:acc)

{-
正の整数を取り、同じ数字を含む次の小さな正の整数を返す関数を書く。

以前、逆の関数を書いたことがある。
その時の解答は以下のような形だった。

module NextBigger (nextBigger) where

import Data.List

nextBigger :: Int -> Int
nextBigger n
  | n == (read $ reverse $ sort $ reverse $ show n) = -1 -- 逆にして並べ替えても同じ場合は該当なし。
  | otherwise = read $ reverse $ next (reverse $ show n) [] -- 逆にして次の値を求め、また逆にする。

next [] acc = acc
next [x] acc = x:acc
next (x:y:xs) acc
  | x > y = let
      m = minimum $ filter (y<) (x:acc)
      r = reverse $ sort $ y : delete m (x:acc)
      in r ++ m : xs
  | otherwise = next (y:xs) (x:acc)

nextの詳細を忘れてしまったが、これでうまく行っているようなので、
逆の関数であるprevを実装。
その結果、0が先頭に来る場合だけNothingになるようにした。

-}
