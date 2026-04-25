-- https://www.codewars.com/kata/55e2adece53b4cdcb900006c
-- Tortoise racing

module Codewars.G964.Tortoise where

race :: Int -> Int -> Int -> Maybe (Int, Int, Int)
race v1 v2 g =
    if v2 <= v1
        then Nothing
        else Just (hours, minutes, seconds)
    where
        totalSeconds = g * 3600 `div` (v2 - v1)
        hours = totalSeconds `div` 3600
        minutes = (totalSeconds `mod` 3600) `div` 60
        seconds = totalSeconds `mod` 60

