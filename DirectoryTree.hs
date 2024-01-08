-- https://www.codewars.com/kata/5ff296fc38965a000963dbbd
-- Directory tree

{-# LANGUAGE OverloadedStrings #-}
module DirectoryTree (tree) where

import Data.Text (Text, splitOn, append, isSuffixOf, empty, unpack)
import Data.List (sort)

data Tree = Directory {name :: Text, childs :: [Tree]} | File {name :: Text} deriving (Eq, Ord, Show)

isFile :: Tree -> Bool
isFile (File _) = True
isFile _ = False

isDirectory :: Tree -> Bool
isDirectory = not . isFile

-- Kata
tree :: Text -> [Text] -> [Text]
tree root files = showTree $ sortTree $ foldl addPath (Directory root []) (map splitPath files)

splitPath :: Text -> [Text]
splitPath = splitOn "/"

toTree :: [Text] -> Tree
toTree [x] = File x
toTree (x:xs) = Directory x [toTree xs]

mkdir :: Text -> Tree
mkdir x = Directory x []

addPath :: Tree -> [Text] -> Tree
addPath t [x] = Directory (name t) (File x : childs t)
addPath t (x:xs) = let
    search = [ c | c <- childs t, isDirectory c, name c == x ]
    in Directory (name t) (
        if search == []
            then addPath (mkdir x) xs : childs t
            else [ (name c == x) ? (addPath c xs, c) | c <- childs t ]
    )

sortTree :: Tree -> Tree
sortTree (Directory s xs) = Directory s (sort [ sortTree x | x <- xs ])
sortTree t = t

showTree :: Tree -> [Text]
showTree (Directory x []) = [x]
showTree (Directory x ts) = let
    initc = concatMap (showDir empty False) $ init ts
    lastc = showDir empty True $ last ts
    in x : initc ++ lastc

showDir :: Text -> Bool -> Tree -> [Text]
showDir header isLast (File x) = [showLine header isLast x]
showDir header isLast (Directory x ts) = let
    newHeader = header & (isLast ? ("    ", "│   "))
    initc = concatMap (showDir newHeader False) $ init ts
    lastc = showDir newHeader True $ last ts
    in (showLine header isLast x) : initc ++ lastc

showLine :: Text -> Bool -> Text -> Text
showLine header isLast name = header & (isLast ? ("└── ", "├── ")) & name

(&) = append
b ? (t, e) = if b then t else e

{-

textT = "+-- " :: Text
textL = "`-- " :: Text
textI = "|   " :: Text
textO = "    " :: Text


ツリーどうしの合成の仕方を考える。
追加したいものと同じディレクトリがある場合、
その下に対して再帰呼び出し。
なければそのまま追加。

"meetings/2021-01-12/notes.txt"
  -> ["meetings","2021-01-12","notes.txt"]

Directory (root, [])

ツリーの形状について。
同じ階層のディレクトリ・ファイルについては、最後だけ"└── "、それ以外は"├── "がファイル名のすぐ手前につく。
その一つ上の階層は、最後なら"    "、最後以外は"│   "がつく。

Desktop
├── meetings
│   ├── 2021-01-12
│   │   ├── notes.txt
│   │   └── report.pdf
│   ├── 2021-01-24
│   │   └── report.pdf
│   └── 2020_calendar.xlsx
├── misc
│   └── photos
│       ├── forest_20130430.jpg
│       └── sunset_20130412.jpg
└── scripts
    └── tree.py
sort $ map (toTree . splitPath) files
[Directory "meetings" [Directory "2021-01-12" [File "notes.txt"]]
,Directory "meetings" [Directory "2021-01-12" [File "report.pdf"]]
,Directory "meetings" [Directory "2021-01-24" [File "report.pdf"]]
,Directory "meetings" [File "2020_calendar.xlsx"]
,Directory "misc" [Directory "photos" [File "forest_20130430.jpg"]]
,Directory "misc" [Directory "photos" [File "sunset_20130412.jpg"]]
,Directory "scripts" [File "tree.py"]]

*DirectoryTree> sortTree $ foldl addPath (Directory root []) (map splitPath files)
Directory {name = "Desktop", childs = [
    Directory {name = "meetings", childs = [
        Directory {name = "2021-01-12", childs = [
            File {name = "notes.txt"},
            File {name = "report.pdf"}]},
        Directory {name = "2021-01-24", childs = [
            File {name = "report.pdf"}]},
        File {name = "2020_calendar.xlsx"}]},
    Directory {name = "misc", childs = [
        Directory {name = "photos", childs = [
            File {name = "forest_20130430.jpg"},
            File {name = "sunset_20130412.jpg"}]}]},
    Directory {name = "scripts", childs = [
        File {name = "tree.py"}]}]}

-}
files :: [Text]
files =
    [ "meetings/2021-01-12/notes.txt"
    , "meetings/2020_calendar.xlsx"
    , "meetings/2021-01-12/report.pdf"
    , "misc/photos/forest_20130430.jpg"
    , "misc/photos/sunset_20130412.jpg"
    , "scripts/tree.py"
    , "meetings/2021-01-24/report.pdf"
    ]

root = "Desktop" :: Text

sampleFile = File "filename"
sampleDir = Directory "dirname" [sampleFile]

run = mapM_ putStrLn $ map unpack $ tree root files
