-- https://www.codewars.com/kata/693e7afc20e67d6a9ebdac5a
-- City Surface Area

module CitySurfaceArea (area) where

area :: [[Int]] -> Int
area grid = rows * cols + 4 * sumH - 2 * sumMinAdjacent
  where
    rows = length grid
    cols = if null grid then 0 else length (head grid)
    sumH = sum [h | row <- grid, h <- row]
    sumMinAdjacent = sumHorizontal + sumVertical
      where
        sumHorizontal = sum [min (row !! j) (row !! (j+1)) | row <- grid, j <- [0..cols-2]]
        sumVertical = sum [min (grid !! i !! j) (grid !! (i+1) !! j) | i <- [0..rows-2], j <- [0..cols-1]]

{-
与えられるのは、都市の高さを表す2次元のリストです。
各要素は、その位置に建物がある場合はその高さを、建物がない場合は0を表します。
都市の表面積を計算するためには、以下のルールに従います：
1. 各建物の上面は1平方単位の表面積を持ちます。
2. 各建物の側面は、隣接する建物の高さの差に基づいて表面積を持ちます。
   隣接する建物がない場合、側面の表面積は建物の高さに等しくなります。
3. 都市の外側に面する側面も表面積に含まれます。
4. 建物の底面は表面積に含まれません。
5. 建物がない土地は表面積に含みます。
このルールに従って、都市の表面積を計算する関数areaを実装してください。

例:
area [[0,0,0],[0,1,0],[0,0,0]] == 13
area [[1,0,1],[0,1,0],[1,0,1]] == 29
area [[1,2,3]] == 21

地面の上面: rows * cols（全てのセルで1）。
建物の上面: 建物があるセル（h > 0）の数だけ追加で1。
側面: 各建物の高さ h に対して4方向の側面を考慮し、隣接する建物との重なりを引く。
総側面面積 = 4 * (全建物の高さの合計) - 2 * (隣接ペアの min(h1, h2) の合計)。
隣接ペア: 水平（左右）と垂直（上下）の隣接セル。
総表面積 = (rows * cols) + 4 * sumH - 2 * sumMinAdjacent

sumH: 全セルの高さの合計。
sumMinAdjacent: 全ての隣接ペア（水平 + 垂直）での min(h1, h2) の合計。
-}