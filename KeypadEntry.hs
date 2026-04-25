-- https://www.codewars.com/kata/54a2e93b22d236498400134b
-- Multi-tap Keypad Text Entry on an Old Mobile Phone

module Haskell.Codewars.KeypadEntry where

import Data.Char (toUpper)

presses :: String -> Int
presses = sum . map pressCount . map toUpper
  where
    keypadGroups = [ "1", "ABC2", "DEF3", "GHI4", "JKL5", "MNO6", "PQRS7", "TUV8", "WXYZ9", "*", " 0", "#" ]
    keypad = concatMap (\g -> zip g [1..]) keypadGroups
    pressCount c = maybe 0 id (lookup c keypad)

{-
説明：古い携帯電話のマルチタップキーパッドを使用してテキストメッセージを入力するために必要なキー押下回数を計算します。
各文字は特定のキーに対応しており、本実装では同じキーを連続して押す場合の一時停止はカウントせず、押下回数のみを合計します。
例：
presses "LOL"        -->  9
presses "HOW R U"    -->  13
presses "WHERE DO U WANT 2 MEET L8R"  -->  47

-}