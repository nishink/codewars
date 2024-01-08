-- https://www.codewars.com/kata/58599c35994864e9ee00017f
-- Open Knight's Tour problem

module Kata.KnightTour where

type Dim = (Int,Int) -- Dimensions of the chessboard (width,height)
type Pos = (Int,Int) -- Position (x,y), 1 based, (1,1) being top left corner
type Solution = [Pos] -- Solution is a list of positions

--https://wiki.haskell.org/99_questions/Solutions/91
solve :: Dim -> Pos -> Maybe Solution
solve (w,h) (x,y)
    | x <= 0 || w < x || y <= 0 || h < h = Nothing
    | otherwise = let
    result = loop (w*h) [[(x,y)]]
    in if null result then Nothing else Just (head result)
    where loop 1 = map reverse . id
          loop i = loop (i-1) . concatMap nextMoves

          nextMoves already@(x:xs) = [next:already | next <- possible]
              where possible = filter (\x -> on_board x && (x `notElem` already)) $ jumps x

          jumps (x,y)    = [(x+a, y+b) | (a,b) <- [(1,2), (2,1), (2,-1), (1,-2), (-1,-2), (-2,-1), (-2,1), (-1,2)]]
          on_board (x,y) = (x >= 1) && (x <= w) && (y >= 1) && (y <= h)

{-
Haskell 99の91問目より解き方を参照。
-}