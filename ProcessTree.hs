-- https://www.codewars.com/kata/52817f04b70058a1b1000037
-- Process trees

module ProcessTree where

import Data.List

type PID = Int
data Process = Process PID [Process] deriving Show

makeTree :: [(PID, PID)] -> Process
makeTree xs = let
    (p:ps) = sortOn snd xs
    root = Process (fst p) []
    in foldl addTree root (sort ps)

addTree :: Process -> (PID, PID) -> Process
addTree (Process pid childs) (cpid, ppid)
    | pid == ppid = Process pid (childs ++ [Process cpid []])
    | otherwise   = Process pid [ addTree cp (cpid, ppid) | cp <- childs ]

{-
makeTree :: [(PID, PID)] -> Process
makeTree [(pid, _)] = Process pid []
makeTree xs = let
    ((pid,_):ps) = sortOn snd xs
    pz = filter ((/=pid) . snd) ps
    childs = [ makeTree (p:pz) | p <- sort $ filter ((==pid) . snd) ps ]
    in Process pid childs

-}




processes :: [(PID, PID)]
processes = [
    (1, -1),
    (219, 214),
    (214, 1),
    (124, 1)
  ]