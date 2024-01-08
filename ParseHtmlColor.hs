-- https://www.codewars.com/kata/58b57ae2724e3c63df000006
-- Parse HTML/CSS Colors

module Codewars.Lambda4fun.ParseHtmlColor where

--import Codewars.Lambda4fun.ParseHtmlColor.PresetColors (presetColors)
import Data.Map.Strict (Map, fromList, (!))

import Data.Char

parseHtmlColor :: String -> Map Char Int
parseHtmlColor ('#':r:g:b:[]) = parseHtmlColor ('#':r:r:g:g:b:b:[])
parseHtmlColor ('#':r1:r2:g1:g2:b1:b2:[]) = fromList [
    ('r',digitToInt r1 * 16 + digitToInt r2),
    ('g',digitToInt g1 * 16 + digitToInt g2),
    ('b',digitToInt b1 * 16 + digitToInt b2)
    ]
parseHtmlColor color = parseHtmlColor $ presetColors ! (map toLower color)

presetColors = fromList [("red","#FF0000"),("green","#00FF00"),("blue","#0000FF")]

{-
HTML/CSSで使われる、#FFFFFFのような色指定。
#ABC -> #AABBCC と解釈する。
-}