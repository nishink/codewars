-- https://www.codewars.com/kata/6363b0c4a93345115c7219cc
-- A self-descriptive fractal sequence

module SequenceA112382 where

import Data.List

a112382 :: Int -> Int
a112382 n = (g 6 [1,1]) !! n
  where
    f [] _ = []
    f (x:xs) ys = (take x ys) ++ [x] ++ f xs (drop x ys)
    g 0 xs = xs
    g n xs = g (n-1) (f xs [1..])

{-
a112382 :: Int -> Int
a112382 n = f [1,1]
  where
    f xs = if length xs <= n then f (addSeq xs) else xs !! n

addSeq :: [Int] -> [Int]
addSeq xs = f xs [1..]
  where
    f [] _ = []
    f (x:xs) ys = (take x ys) ++ [x] ++ f xs (drop x ys)

repeatSeq :: Int -> [Int]
repeatSeq n = f n [1,1]
  where
    f 0 xs = xs
    f n xs = f (n-1) (addSeq xs)
-}

{-
-- 関数 a112382
a112382 :: Int -> Int
a112382 n = nest [1,1] n !! n
  where
    nest :: [Int] -> Int -> [Int]
    nest list 0 = list
    nest list x = nest (flatten list) (x-1)

flatten :: [Int] -> [Int]
flatten list = let
  m = maximum list
  l = length $ uppertrim list
  in list ++ [(m + 1)..(m + list !! l)] ++ [list !! l]

-- listから、1〜listの最大値について最初に出現したものを削除する
uppertrim :: [Int] -> [Int]
uppertrim list = list \\ [1..maximum list]
--uppertrim list = foldl deleteCases list [1..maximum list]

-- listからelemの最初の出現を削除する
deleteCases :: [Int] -> Int -> [Int]
deleteCases list elem = let
  (f, s) = span (/=elem) list
  in f ++ tail s
-}

{-
全く解法が思いつかないので全面的にChatGPTに頼る。

この問題を解決するためには、OEIS A112382で説明されている自己記述的なフラクタル数列を生成する必要があります。
この数列は各正の整数をちょうど一度含み、最初に現れる各整数を削除した後に再帰的に自分自身を含むプロパー部分列を持っています。

アプローチ
この数列を生成して、効率的にn番目の項を見つけるには、数列がどのように構築されるかを理解する必要があります：

数列は1, 1で始まります。
数列の各数a(n)は、その位置までに出現した整数の最初の出現の数を表します。
大きな値（最大で100,000）に対して効率的に実装するためには、数列を保持し、最初の出現のカウントを追跡する戦略をとります。

実装手順
初期化:

初期の数列を[1]として始め、最初の出現を記録する空のリストを用意します。
数列を生成するループ:

数列の各位置iについて、その位置までに出現した整数の最初の出現の数を決定します。
このカウントを数列に追加します。
もしこの新しい要素がその数の最初の出現であれば、最初の出現リストを更新します。
効率的なルックアップ:

各数のカウントを効率的に決定するために辞書（マップ）を使用します。

コードの説明
データ構造:

各数のカウントを効率的に保存および更新するために、Data.IntMapを使用します。
iterate関数:

iterate関数を使用して、数列をステップごとに構築します。
next関数は、現在の位置までの整数の最初の出現数を決定することで数列の次の要素を計算します。
効率的な更新とルックアップ:

IM.insertWith (+)を使用して各数のカウントをインクリメントし、効率的に最初の出現を追跡します。
このアプローチにより、数列が必要な項まで効率的に生成され、大きな値のnに対しても対応できます。


なるほど全然わからん。
解き方として提示されている、MATHEMATICAのコードを読み解いてみる。

uppertrim[list_]:= Fold[ DeleteCases[#1, #2, 1, 1]&, list, Range[ Max[list] ] ]; 
Nest[
  Flatten[ 
    Append[ 
      #, 
      Append[ 
        Range[ 
          Max[#] + 1, 
          Max[#] + #[ [ Length[uppertrim[#]] + 1 ] ]
        ], 
        #[ [ Length[uppertrim[#]] + 1 ] ]
      ]
    ]
  ] &, 
  {1, 1}, 
  10
] 

Rangeは、1から指定した数のリストを表す。
Range[10] = [1,2,3,4,5,6,7,8,9,10]
Rangeに複数の引数を指定した場合は以下のとおり。
Range[初期値,終端値,間隔]
Range[10,20,2] = [10,12,14,16,18,20]

Maxは、リスト要素の最大値。

DeleteCasesは、リストからパターンに合うものを取り除く。
#1, #2は引数で、次に書いてあるlistやRange[ Max[list] ]のことを表している。

Foldは指定した関数を繰り返し適用するという意味合いになるようだ。
Fold[function, {x1, x2, x3}, value] = function[x1,function[x2,function[x3,value]]] 

これの通りに実装してみたが、
問題文にあるような100000を指定すると、計算が終わらなかった。
このアプローチは違うらしい。


最初に与える[1,1]から、次の値を導き出すことができるらしい。
そのルールさえわかれば解けるとのこと。

1, 1, 2, 1, 3, 4, 2, 5, 1, 6, 7, 8, 3, 9, 10, 11, 12, 4, 13, 14, 2, 15, 16, 17, 18, 19, 5, 20, 1, 21, 22, 23, 24, 25, 26, 6, 27, 28, 29, 30, 31, 32, 33, 7, 34, 35, 36, 37, 38, 39, 40, 41, 8, 42, 43, 44, 3, 45, 46, 47, 48, 49, 50, 51, 52, 53, 9, 54, 55, 56, 57, 58, 59, 60
X, 1, X, 1, X, X, 2, X, 1, X, X, X, 3, X,  X,  X,  X, 4,  X,  X, 2,  X,  X,  X,  X,  X, 5,  X, 1,  X,  X,  X,  X,  X,  X, 6,  X,  X,  X,  X,  X,  X,  X, 7,  X,  X,  X,  X,  X,  X,  X,  X, 8,  X,  X,  X, 3,  X,  X,  X,  X,  X,  X,  X,  X,  X, 9,  X,  X,  X,  X,  X,  X,  X

1, 1
X, 1, X, 1
1, 1, 2, 1
X, 1, X, 1, X, X, 2, X, 1
1, 1, 2, 1, 3, 4, 2, 5, 1

初期値を[1,1]として、各要素の前に、各要素の数だけ数列をくっつける。
これを繰り返して数列を作っていく、という方法はどうだろうか。

やってみたが、長さが27000を超えたあたりで急に処理が終わらなくなる。

-}