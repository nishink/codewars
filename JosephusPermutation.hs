-- https://www.codewars.com/kata/5550d638a99ddb113e0000a2
-- Josephus Permutation

module Josephus where

josephus :: [a] -> Int -> [a]
josephus xs k = josephus' xs []
  where
    josephus' [] ys = ys
    josephus' xs ys = let
        k' = k `mod` length xs
        nextXs = if k' == 0 then init xs else drop k' xs ++ take (k' - 1) xs
        nextYs = if k' == 0 then ys ++ [last xs] else ys ++ [xs !! (k' - 1)]
        in josephus' nextXs nextYs

{-
この問題は、間違いなく古代歴史家ヨセフスの人生で最も重要な出来事にちなんで名付けられました。
彼の話によると、彼と彼の40人の兵士は、包囲中にローマ人によって洞窟に閉じ込められました。

敵に降伏することを拒否し、彼らは代わりに集団自殺を選びました。
彼らは輪を形成し、最後の1人が残るまで、3人に1人を殺しました（そして、行為を終わらせるために自殺することになっていた）。

さて、ヨセフスともう一人の男が最後の二人で、私たちは今物語のすべての詳細を知っているので、
あなたは彼らが当初のアイデアを正確に実行しなかったと正しく推測したかもしれません。

今度は、Josephusの順列を返す関数を作成し、パラメータとして、項目の最初の配列/リストを円のように順列し、
何も残らないまでkか所ごとにカウントします。

ヒントと注意事項：通常の0からn-1の範囲ではなく、1からnまでのカウントを開始するのに役立ちます。kは常に>=1になります。

たとえば、array=[1,2,3,4,5,6,7]とk=3の場合、関数はこのように動作するはずです。

[1,2,3,4,5,6,7] - initial sequence
[1,2,4,5,6,7] => 3 is counted out and goes into the result [3]
[1,2,4,5,7] => 6 is counted out and goes into the result [3,6]
[1,4,5,7] => 2 is counted out and goes into the result [3,6,2]
[1,4,5] => 7 is counted out and goes into the result [3,6,2,7]
[1,4] => 5 is counted out and goes into the result [3,6,2,7,5]
[4] => 1 is counted out and goes into the result [3,6,2,7,5,1]
[] => 4 is counted out and goes into the result [3,6,2,7,5,1,4]
したがって、私たちの最終結果は：

[3,6,2,7,5,1,4]

解き方：
リストの先頭から順に、k番目の要素を取り出して、結果リストに追加する。
リストから取り出した要素は、元のリストから削除する。
元のリストが空になるまで繰り返す。

リストは循環リストとして扱う。
すなわち、k番目の要素を取り除いた後は、k+1番目の要素を先頭にして、k-1番目の要素を末尾にする。
リストの長さがk未満の場合、kをリストの長さで割った余りをkとする。

-}