-- https://www.codewars.com/kata/52f831fa9d332c6591000511
-- Molecule to atoms

module MoleculeToAtoms where

import Data.Char

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = let
    tokens = tokenize formula
    in if lint tokens
        then Right (summarize $ parse tokens [])
        else Left "Not a valid molecule"

tokenize :: String -> [String]
tokenize [x] = [[x]]
tokenize s@(x:y:xs)
    | isUpper x && isLower y = [x,y] : tokenize xs
    | isNumber x = takeWhile isNumber s : tokenize (dropWhile isNumber s)
    | otherwise = [x] : tokenize (y:xs)

lint :: [String] -> Bool
lint [] = True
lint (t:ts)
    | t == "(" = bracket ")"
    | t == "[" = bracket "]"
    | t == "{" = bracket "}"
    | isLower (head t) = False
    | otherwise = lint ts
    where
        bracket close = let 
            inside = takeWhile (/= close) ts
            outside = dropWhile (/= close) ts
            in inside /= [] && outside /= []

parse :: [String] -> [(String,Int)] -> [(String,Int)]
parse [] before = before
parse (t:ts) before
    | t == "(" = bracket ")"
    | t == "[" = bracket "]"
    | t == "{" = bracket "}"
    | and (map isNumber t) = multiply before (read t) ++ parse ts []
    | otherwise = before ++ parse ts [(t,1)]
    where
        bracket close = let
            inside = takeWhile (/= close) ts
            outside = tail $ dropWhile (/= close) ts
            in before ++ parse outside (parse inside [])

multiply :: [(String,Int)] -> Int -> [(String,Int)]
multiply atoms n = [ (s,i*n) | (s,i) <- atoms ]

summarize :: [(String,Int)] -> [(String,Int)]
summarize atoms = calc atoms []
    where
        calc [] acc = acc
        calc (a:as) acc = let
            newacc = if find a acc then 
                    [ if fst a == s then (s,i + snd a) else (s,i) | (s,i) <- acc ]
                    else acc ++ [a]
            in calc as newacc
        find a acc = or [ fst a == s | (s,_) <- acc ]
{-
分子式の解析
まずトークンごとに分ける。
"Mg(OH)2" -> "Mg","(","O","H",")","2"
先頭から順にカウントする
[("Mg",1)]
括弧が出てきたら、括弧の中をまず計算する。
[("O",1),("H",1)]
数字が出てきたら、直前の要素数を掛け算する。
[("O",2),("H",2)]

"("がきたら")"までの要素を取得
数字がきたら、直前の要素に掛け算
それ以外は加算する。

-}

