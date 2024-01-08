-- https://www.codewars.com/kata/52742f58faf5485cae000b9a
-- Human readable duration format

module FormatDuration where

formatDuration :: (Integral i) => i -> String
formatDuration 0 = "now"
formatDuration n = let
    (y, r1) = divMod (toInteger n) year
    (d, r2) = divMod r1 day
    (h, r3) = divMod r2 hour
    (m, s)  = divMod r3 minute
    in connect $ format [(y,"year"),(d,"day"),(h,"hour"),(m,"minute"),(s,"second")]

format [] = []
format ((i,s):xs) 
    | i == 0 = format xs
    | i == 1 = ("1 " ++ s) : format xs
    | otherwise = (show i ++ " " ++ s ++ "s") : format xs

connect [] = []
connect [x] = x
connect (x:xs) =
    if length xs == 1
        then x ++ " and " ++ connect xs
        else x ++ ", " ++ connect xs

minute = 60
hour = minute * 60
day = hour * 24
year = day * 365

