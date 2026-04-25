-- https://www.codewars.com/kata/526989a41034285187000de4
-- Count IP Addresses

module IPsBetween where

type IPv4 = String

ipsBetween :: IPv4 -> IPv4 -> Int
ipsBetween ip1 ip2 = convertIP2Int ip2 - convertIP2Int ip1

convertIP2Int :: IPv4 -> Int
convertIP2Int ip = foldl1 (\a b -> a * 256 + b) $ map read $ split '.' ip
    where
        split sep str = words [ if c == sep then ' ' else c | c <- str ]

{-
IPアドレスを数える。
xxx.xxx.xxx.xxxを数値に変換し、差分を取れば良さそう。
.での分割はsplitOn、数値にはreadで変換できる。
-}

