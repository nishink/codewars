-- https://www.codewars.com/kata/5632e12703e2037fa7000061
-- Base64 Numeric Translator

module Codewars.Kata.Base64 where

import Data.List

base64ToBase10 :: String -> Integer
base64ToBase10 str = foldl1 (\x y -> x * 64 + y) (map translate str) where
    translate c = fromIntegral $ head $ elemIndices c base64characters
    base64characters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

{-
BASE64を10進数に変換。
base64charactersにBASE64で使う文字列を並べて、
変換対象の文字列を１つずつ取り出し、indexを取得。
64をかけながら加算。
-}