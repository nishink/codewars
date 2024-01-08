-- https://www.codewars.com/kata/515bb423de843ea99400000a
-- PaginationHelper

module Codewars.Kata.Pagination where

type Collection a = [a]
type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length

pageCount :: Collection a -> ItemsPerPage -> Int
pageCount xs n = let
    (d, m) = divMod (length xs) n
    in if m == 0 then d else d + 1

pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount xs n page
    | page < 0 = Nothing
    | d > page = Just n
    | d == page && m > 0 = Just m
    | otherwise = Nothing
    where
        (d, m) = divMod (length xs) n

pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex xs n item
    | item < 0 = Nothing
    | item >= length xs = Nothing
    | otherwise = Just (item `div` n)
