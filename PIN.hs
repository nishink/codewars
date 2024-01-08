-- https://www.codewars.com/kata/5263c6999e0f40dee200059d
-- The observed PIN

module PIN where

getPINs :: String -> [String]
getPINs [] = []
getPINs [x] = [ [a] | a < adjacent x ]
getPINs (x:xs) = [ a:as | a <- adjacent x, as <- getPINs xs ]

adjacent :: Char -> [Char]
adjacent '0' = "08"
adjacent '1' = "124"
adjacent '2' = "1235"
adjacent '3' = "236"
adjacent '4' = "1457"
adjacent '5' = "24568"
adjacent '6' = "3569"
adjacent '7' = "478"
adjacent '8' = "57890"
adjacent '9' = "689"