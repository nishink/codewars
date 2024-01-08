-- https://www.codewars.com/kata/5839c48f0cf94640a20001d3
-- Land perimeter

module Kata (landPerimeter) where

landPerimeter :: [String] -> String
landPerimeter area = let
    height = length area
    width = length $ head area
    perimeter area x y = case area !! y !! x of
        'O' -> 0
        'X' -> 
            (if y == 0 then 1 else if 'O' == area !! (y-1) !! x  then 1 else 0) + 
            (if y == height-1 then 1 else if 'O' == area !! (y+1) !! x then 1 else 0) + 
            (if x == 0 then 1 else if 'O' == area !! y !! (x-1) then 1 else 0) + 
            (if x == width-1 then 1 else if 'O' == area !! y !! (x+1) then 1 else 0)
    total = sum [ perimeter area x y | x <- [0..width-1], y <- [0..height-1] ]
    in "Total land perimeter: " ++ show total



{-
島と海の境界線を数える。

['XOOXO',
 'XOOXO',
 'OOOXO',
 'XXOXO',
 'OXOOO'] 

先頭から１マスずつ見ていき、
そのマスが島(X)の場合、四方を見て隣が何もないか海(O)の場合は1、
隣が島(X)の場合は0でカウントすればいい。
海域全体の大きさは正方形とは限らないようなので、まずは幅と高さを求める。

-}