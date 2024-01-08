-- https://www.codewars.com/kata/5b94d7eb1d5ed297680000ca
-- Simple directions reversal

module SimpleDirections where 

solve :: [String] -> [String]
solve xss = let
    drs = [ (x, xs) | (x:y:xs) <- map words xss ]
    roads = map snd $ reverse drs
    ds = map fst $ reverse drs
    dirs = last ds : (map switch $ init ds)
  in [ unwords (dir:"on":road) | (dir, road) <- zip dirs roads ]

switch "Right" = "Left"
switch "Left" = "Right"

