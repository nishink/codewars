-- https://www.codewars.com/kata/696fe8b06b4e2e6ddb50caa8
-- Drawing English Ruler

module EnglishRuler (draw) where

draw :: Int -> Int -> String
draw t n = 
    unlines $ drawRuler t n
  where
    drawRuler :: Int -> Int -> [String]
    drawRuler tickLength inches = concatMap (drawInch tickLength) [0..inches]
    
    drawInch :: Int -> Int -> [String]
    drawInch tickLength inch
      | inch == 0 = [drawTick tickLength inch]
      | otherwise = drawTicks (tickLength - 1) ++ [drawTick tickLength inch]
    
    drawTicks :: Int -> [String]
    drawTicks 0 = []
    drawTicks len = drawTicks (len - 1) ++ [drawTick len (-1)] ++ drawTicks (len - 1)
    
    drawTick :: Int -> Int -> String
    drawTick len label
      | label >= 0 = replicate len '-' ++ " " ++ show label
      | otherwise  = replicate len '-'
{-
A standard English ruler is marked in inches. 
For each whole inch, the ruler shows a major tick with a numeric label. 
Each time the interval size is halved, the tick length decreases by 1. 
The length of the major tick t defines the scale of the ruler.

Write a function that generates the ASCII drawing of a ruler.

t - major tick length
n - number of inches on the ruler
Examples

t=2 (n=4)      t=3 (n=3)        t=4 (n=2)         t=5 (n=1)
-- 0           --- 0            ---- 0            ----- 0
-              -                -                 -
-- 1           --               --                --
-              -                -                 -
-- 2           --- 1            ---               ---
-              -                -                 -
-- 3           --               --                --
-              -                -                 -
-- 4           --- 2            ---- 1            ----
               -                -                 -
               --               --                --
               -                -                 -
               --- 3            ---               ---
                                -                 -
                                --                --
                                -                 -
                                ---- 2            ----- 1
-}