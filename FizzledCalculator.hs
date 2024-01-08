-- https://www.codewars.com/kata/551d9695bf4e5283c70005b9
-- The Student and the Fizzled Calculator (Check whether a product is smaller than a certain number)

module Codewars.Kata.FizzledCalculator where
import Prelude hiding ((*), (/), product)

-- | Should return True if x * y < z, otherwise False.
fizzledCalculator :: Double -> Double -> Double -> Bool
fizzledCalculator x y z =
  if x == 0 || y == 0 then 0 < z else 
  if z == 0 then (x < 0 && y > 0) || (x > 0 && y < 0) else
  if z > 0 && ((x < 0 && y > 0) || (x > 0 && y < 0)) then True else
  if z < 0 && ((x < 0 && y < 0) || (x > 0 && y > 0)) then False else
  if x < 0 && y < 0 && z > 0 then exp (log (negate x) + log (negate y)) < z else 
  if x < 0 && y > 0 && z < 0 then exp (log (negate x) + log y) >= negate z else
  if x > 0 && y < 0 && z < 0 then exp (log x + log (negate y)) >= negate z else
  exp (log x + log y) < z -- x * y = exp (log x + log y); if x > 0 && y > 0

{-
* / productを使わず、x * y < zを証明する。
x y zの正負だけで判別できるものをまず並べてみたが、それだけでは足りない。
ChatGPTに、掛け算記号を使わずに掛け算を表す方法がないか聴いてみたところ、
x * y = exp (log x + log y)
という式の変形方法を教わった。
あとはその応用。

-}