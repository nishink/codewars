module Kata.DrawACircle (circle) where

-- circle :: Int -> String
-- circle r = concat [ [ if sqrt (fromIntegral ((r-x)^2 + (r-y)^2)) <= fromIntegral r then '█' else ' ' | x <- [0..(r-1)*2] ] ++ "\n" | y <- [0..(r-1)*2] ]

import Data.List (intercalate)

-- circle function
circle :: Int -> String
circle radius
    | radius < 0  = ""
    | radius == 0 = "\n"
    | otherwise   = unlines $ trim [generateRow y | y <- [0..size-1]]
  where
    size = 2 * radius + 1  -- grid size
    center = fromIntegral radius
    generateRow y = trim [generateChar x y | x <- [0..size-1]]
    generateChar x y
        | distanceFromCenter x y < fromIntegral radius = '\x2588' -- Unicode block character
        | otherwise = ' '
    distanceFromCenter x y =
        sqrt ((fromIntegral x - center) ^ 2 + (fromIntegral y - center) ^ 2)

trim = init . tail
{-
     █████████     
    ███████████    
  ███████████████  
  ███████████████  
 █████████████████ 
███████████████████
███████████████████
███████████████████
███████████████████
███████████████████
███████████████████
███████████████████
███████████████████
███████████████████
 █████████████████ 
  ███████████████  
  ███████████████  
    ███████████    
     █████████     

円はスペースと文字 \u2588 で構成されます。
これは文字の完全な正方形であるため、「ギャップ」はスペースで埋められていることに注意してください。
たとえば、上記の最後の行は 5 文字「\u2588 」で終わります。最後にスペースが5つあります。

中心からの距離が指定された半径より小さいすべての文字は「円内」として定義されるため、
その位置の文字はスペースではなく \u2588 で埋める必要があります。

--

例えば10を指定したら、(10,10)を中心とした21x21の文字列配列を作成し、
中心からの距離が半径以内なら\u2588、そうでないならスペースで埋める。


-}