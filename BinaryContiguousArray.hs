-- https://www.codewars.com/kata/60aa29e3639df90049ddf73d/haskell
-- Binary Contiguous Array

module BinaryContiguousArray (binarray) where

import qualified Data.Map.Strict as M

binarray :: [Int] -> Int
binarray nums = f nums 0 0 (M.fromList [(0,-1)]) 0
    where
        f [] _ _ _ maxLen = maxLen
        f (n:ns) index count countMap maxLen = let
            newCount = count + if n == 0 then -1 else 1
            in if M.member newCount countMap
                then f ns (index+1) newCount countMap (max maxLen (index - (countMap M.! newCount)))
                else f ns (index+1) newCount (M.insert newCount index countMap) maxLen


{-
0と1の配列から、0と1の数が等しい最長の部分列の長さを求める。
まずChatGPTに聴いてみると、pythonで解いて見せてくれた。
これをHaskellで実装しなおす。

def findMaxLength(nums):
    count = 0
    maxLen = 0
    count_dict = {0: -1}

    for i in range(len(nums)):
        if nums[i] == 0:
            count -= 1
        else:
            count += 1

        if count in count_dict:
            maxLen = max(maxLen, i - count_dict[count])
        else:
            count_dict[count] = i

    return maxLen

ChatGPTは以下のような解説をしてくれた。

1. 変数 maxLen を初期化して、等しい数の0と1からなる最長の連続した部分配列の長さを格納します。
2. 与えられたバイナリ配列を走査し、これまでに遭遇した0と1の数の差分のランニングカウントを維持します。
   ハッシュマップ（Haskellでは連想リストとして表現）を使用して、
   ランニングカウントと対応するインデックスを格納します。
3. ランニングカウントが以前に遭遇したランニングカウントと等しくなったとき、
   現在の連続した部分配列の長さを計算し、現在の長さが以前の最大よりも大きい場合は maxLen を更新します。
4. ランニングカウントがゼロになると、これは0と1の数が最初から現在のインデックスまで等しいことを意味します。
   この場合、maxLen を現在のインデックス + 1に更新します。

どうも４の説明だけ間違っているように見える。

後半のif文は、ランニングカウントが過去にも現れたことのあるものである場合に、
最初に現れた時のインデックスと、現在のインデックスの差を計算し、
maxLenより大きい場合はmaxLenを更新している。
ランニングカウントが過去に現れたことのないものである場合は、辞書に新規追加している。

すなわち、ランニングカウントが同じインデックスのものは、0,1の数が同じ部分文字列になるという性質を利用して、
ランニングカウントが最初に現れた時のインデックスを辞書に保存し、
与えられたバイナリ配列を走査していく中で、現在のインデックスと辞書の値の差を取ることで、
部分文字列の最大長を求めている。

-}