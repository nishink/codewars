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




-}