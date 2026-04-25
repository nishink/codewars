-- https://www.codewars.com/kata/55911ef14065454c75000062
-- Multiplying numbers as strings

module MultNumAsStrings where

import Data.Char (intToDigit, digitToInt)

-- | mulitply two numbers as strings
multiply :: String -> String -> String
multiply xs ys = show ((read xs) * (read ys))

