-- https://www.codewars.com/kata/56876fd23475fa415e000031
-- Chess position parser (FEN)

module Chess.FEN (parseFen) where

import Data.Char

parseFen :: String -> String
parseFen fen = let
    fs = words fen
    bs = lines [ if c == '/' then '\n' else c | c <- (fs !! 0) ]
    ps = [ map parseChar l | l <- parseCheckered $ map parseSpace bs ]
    ts = if fs !! 1 == "b" then blackTurn ps else ps
  in unlines ts

blackTurn :: [String] -> [String]
blackTurn ss = reverse [ reverse l | l <- ss ]

parseSpace :: String -> String
parseSpace "" = ""
parseSpace (x:xs) = 
  (if elem x ['1'..'8']
    then replicate (digitToInt x) 'S'
    else [x]) ++ parseSpace xs

parseCheckered :: [String] -> [String]
parseCheckered ss = 
  [ [ if c == 'S' then if odd col == odd row then c else 's' else c
       | (c, col) <- zip l [1..] ] 
       | (l, row) <- zip ss [1..] ]

parseChar :: Char -> Char
parseChar c = case c of
  'S' -> '\x2587' -- White Square
  's' -> '\xFF3F' -- black square
  'K' -> '\x265A' -- White King
  'Q' -> '\x265B' -- White Queen
  'R' -> '\x265C' -- White Rook
  'B' -> '\x265D' -- White Bishop
  'N' -> '\x265E' -- White kNight
  'P' -> '\x265F' -- White Pawn
  'k' -> '\x2654' -- Black king
  'q' -> '\x2655' -- Black queen
  'r' -> '\x2656' -- Black rook
  'b' -> '\x2657' -- Black bishop
  'n' -> '\x2658' -- Black knight
  'p' -> '\x2659' -- Black pawn
