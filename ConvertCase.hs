-- https://www.codewars.com/kata/59be8c08bf10a49a240000b1
-- Convert all the cases!

module ConvertCase where 

import Data.Char
import Data.List
import Data.List.Split

changeCase :: String -> String -> Maybe String
changeCase xs "snake" = do
    xss <- separate xs
    return (convertSnake xss)
changeCase xs "camel" = do
    xss <- separate xs
    return (convertCamel xss)
changeCase xs "kebab" = do
    xss <- separate xs
    return (convertKebab xss)
changeCase _ _ = Nothing

separate :: String -> Maybe [String]
separate xs
    | elem '_' xs && elem '-' xs = Nothing
    | elem '_' xs = if all isLower (filter (/='_') xs) then Just (separateSnake xs) else Nothing
    | elem '-' xs = if all isLower (filter (/='-') xs) then Just (separateKebab xs) else Nothing
    | otherwise = Just (separateCamel xs)

separateSnake :: String -> [String]
separateSnake xs = splitOn "_" xs

separateKebab :: String -> [String]
separateKebab xs = splitOn "-" xs

separateCamel :: String -> [String]
separateCamel xs = words $ sepCamel xs
    where
        sepCamel "" = ""
        sepCamel (x:xs) = if isUpper x then ' ' : (toLower x) : sepCamel xs else x : sepCamel xs

convertSnake :: [String] -> String
convertSnake xss = intercalate "_" xss

convertKebab :: [String] -> String
convertKebab xss = intercalate "-" xss

convertCamel :: [String] -> String
convertCamel [] = []
convertCamel (x:xss) = concat $ x : map (\(y:ys) -> toUpper y : ys) xss
