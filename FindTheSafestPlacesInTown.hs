-- https://www.codewars.com/kata/5dd82b7cd3d6c100109cb4ed
-- Find the safest places in town

module FindTheSafestPlacesInTown (advice) where

import Data.List
import qualified Data.Set as S

import Control.Monad (forM_)
import Data.Array (elems, (!))
import Data.Array.ST
    ( readArray, writeArray, MArray(newArray), runSTArray )


type Coordinate = (Int,Int)

advice :: [Coordinate] -> Int -> [Coordinate]
advice agents n
  | agents' == everywhere n = []
  | null agents' = everywhere n
  | otherwise   = safestPlaces agents' n
  where
    agents' = [ (x, y) | (x, y) <- agents, x >= 0, x < n, y >= 0, y < n ]

everywhere :: Int -> [Coordinate]
everywhere n = [(x, y) | x <- [0..n-1], y <- [0..n-1]]

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

safestPlaces :: [Coordinate] -> Int -> [Coordinate]
safestPlaces agents n = let
  places = safestPlaces' (everywhere n) n []
  m = fst $ maximum places
  in map snd $ filter ((==m).fst) places
  where
    safestPlaces' [] _ result = result
    safestPlaces' ((x, y):rest) n result =
      let minDist = minimum [manhattanDistance (x, y) agent | agent <- agents]
      in safestPlaces' rest n ((minDist,(x, y)):result)


adviceAgents :: [Coordinate] -> Int -> [Coordinate]
adviceAgents [] _ = []
adviceAgents agents n
    | n == 0 = []
    | null agents' = grid
    | agents' == grid = []
    | otherwise = safestPlaces
    where
        grid = everywhere n
        agents' = nub $ filter (inside n) agents
        distances = [(coord, minimum [manhattanDistance coord a | a <- agents']) | coord <- grid]
        maxMinDistance = maximum (map snd distances)
        safestPlaces = map fst (filter ((== maxMinDistance) . snd) distances)

adviceAgent2 :: [Coordinate] -> Int -> [Coordinate]
adviceAgent2 [] _ = []
adviceAgent2 agents n
    | n == 0 = []
    | null agents' = grid
    | agents' == grid = []
    | otherwise = adv grid agents'
    where
        grid = everywhere n
        agents' = nub $ filter (inside n) agents
        adv places ags = let
            remain = places \\ ags
            in if null remain then places else adv remain (neighbour ags n)

neighbour :: [Coordinate] -> Int -> [Coordinate]
neighbour agents n = nub $ concat [ filter (inside n) [ (x+1,y),(x-1,y),(x,y+1),(x,y-1) ] | (x, y) <- agents ]

inside :: Int -> Coordinate -> Bool
inside n (x, y) = x >= 0 && x < n && y >= 0 && y < n

agents::[Coordinate]
agents=[(32,5),(56,53),(30,68),(32,50),(20,1),(64,47),(10,47),(48,8),(51,62),(53,16),(6,44),(71,38),(7,11),(62,20),(41,18),(41,54),(71,0),(10,42),(24,13),(22,30),(41,23),(43,26),(17,56),(19,59),(59,12),(6,49),(15,27),(66,49),(57,53),(10,18),(38,63),(62,27),(42,7),(27,7),(69,64),(55,54),(38,57),(49,72),(17,11),(40,65),(9,33),(3,15),(7,20),(31,45),(15,23),(22,28),(21,65),(51,17),(33,4),(23,16),(19,11),(58,34),(46,58),(66,27),(73,31),(73,21),(20,70),(49,2),(31,55),(10,6),(57,6),(61,7),(20,15),(45,14),(9,14),(70,54),(63,8),(4,23),(69,11),(24,50),(23,7),(32,19),(74,49),(36,55),(51,9),(64,38),(50,48),(8,69),(22,44),(44,17),(59,72),(16,44),(54,19),(61,50),(44,74),(13,30),(29,9),(0,49),(72,52),(11,28),(62,67),(6,24),(50,31),(45,1),(57,29),(47,21),(31,65),(20,63),(16,61),(55,44),(17,45),(63,74),(15,45),(40,56),(21,63),(61,10),(42,5),(43,6),(32,14),(73,70),(51,66),(30,30),(29,41),(29,7),(50,66),(72,57),(51,30),(72,36),(37,65),(53,34),(9,41),(7,67),(28,68),(63,11),(42,34),(64,1),(17,13),(33,64),(1,0),(51,43),(22,32),(45,32),(29,51),(51,34),(56,62),(55,29),(31,37),(17,54),(10,12),(0,27),(37,67),(54,18),(14,42),(25,32),(14,72),(64,43),(18,15),(23,5),(24,35),(59,69),(54,20),(28,70),(0,0),(45,67),(64,63),(2,64),(63,60),(6,56),(44,18),(50,36),(11,36),(64,74),(19,45),(17,12),(49,10),(59,30),(32,22),(50,58),(34,49),(39,33),(59,28),(24,40),(65,64),(33,65),(42,14),(24,64),(24,20),(33,20),(57,33),(67,56),(36,64),(6,10),(69,31),(72,47),(41,3),(36,46),(45,46),(36,27),(39,53),(6,23),(21,12),(27,67),(21,46),(49,14),(22,22),(74,58),(13,39),(31,62),(62,5),(70,71),(51,7),(20,64),(36,23),(72,10),(15,41),(63,50),(20,10),(19,7),(6,13),(52,65),(57,38),(14,61),(5,46),(47,43),(9,18),(47,68),(1,10),(38,13),(67,33),(67,62),(48,16),(6,40),(21,28),(18,64),(73,16),(61,0),(5,23),(47,31),(16,43),(52,68),(53,74),(39,71),(0,45),(34,38),(67,10),(30,20),(71,36),(53,17),(58,61),(29,39),(7,36),(11,53),(46,68),(29,57),(41,32),(37,6),(16,8),(4,51),(59,23),(2,27),(17,53),(41,44),(33,41),(8,35),(7,14),(26,4),(14,60),(20,16),(69,62),(42,15),(6,47),(4,63),(42,39),(30,73),(2,62),(71,41),(1,38),(50,47),(62,26),(57,67),(16,17),(61,48),(45,10),(34,15),(7,41),(41,74),(55,2),(31,21),(52,45),(71,49),(45,68),(59,62),(38,29),(12,21),(31,0),(25,64),(31,9),(67,64),(45,0),(46,31),(57,0),(45,17),(4,3),(20,21),(10,59),(62,50),(57,55),(63,72),(32,7),(67,58),(15,46),(29,54),(20,5),(18,22),(31,8),(34,68),(4,74),(22,65),(57,34),(24,17),(63,14),(27,58),(58,29),(19,37),(50,65),(56,7),(28,50),(69,0),(31,43),(54,37),(66,57),(7,32),(42,27),(67,63),(9,4),(3,46),(15,47),(65,8),(60,33),(31,20),(46,0),(36,47),(39,39),(21,52),(7,16),(68,13),(52,60),(31,2),(66,17),(18,41),(45,21),(26,25),(65,31),(19,42),(46,28),(28,43),(51,2),(51,36),(19,71),(8,15),(0,21),(66,15),(17,7),(72,5),(18,21),(46,9),(63,21),(40,32),(53,56),(23,55),(64,0),(35,61),(0,29),(62,23),(62,39),(41,17),(40,3),(63,54),(44,73),(29,17),(52,17),(42,8),(16,42),(9,20),(66,13),(14,22),(49,4),(43,16),(60,43),(10,25),(8,58),(2,58),(17,44),(32,12),(1,71),(74,2),(72,32),(33,21),(44,28),(11,72),(72,54),(1,7),(52,13),(23,71),(19,38),(54,22),(11,18),(38,74),(12,45),(3,41),(67,21),(47,70),(4,38),(59,47),(32,10),(52,55),(16,57),(64,7),(16,11),(39,37),(73,50),(32,57),(23,33),(2,55),(23,48),(54,55),(9,71),(49,63),(36,18),(47,72),(16,30),(52,51),(73,66),(2,22),(55,41),(66,66),(74,16),(53,66),(45,20),(39,21),(13,36),(8,61),(15,61),(58,53),(8,73),(11,20),(27,18),(46,38),(39,24),(69,8),(38,41),(31,57),(18,47),(57,21),(4,59),(44,10),(32,54),(4,72),(40,48),(28,31),(70,25),(23,0),(17,73),(54,40),(58,43),(15,68),(39,54),(11,61),(51,12),(37,15),(56,65),(3,63),(11,71),(11,30),(9,32),(13,40),(56,42),(51,10),(8,1),(52,16),(50,72),(71,71),(63,29),(4,7),(9,37),(26,21),(2,53),(59,4),(0,13),(5,39),(10,70),(9,24),(43,21),(1,46),(68,30),(66,69),(53,44),(36,63),(54,73),(10,57),(57,51),(33,31),(63,10),(8,32),(17,60),(44,6),(35,62),(17,5),(37,59),(39,40),(7,64),(58,44),(42,47),(19,55),(23,2),(21,51),(61,65),(73,41),(53,57),(32,46),(10,63),(50,27),(37,10),(48,63),(69,68),(49,48),(19,61),(25,9),(33,8),(51,53),(5,45),(12,6),(8,24),(12,12),(14,30),(32,48),(23,9),(71,5),(12,10),(54,69),(36,32),(72,21),(8,44),(42,42),(22,59),(46,15),(52,70),(60,36),(10,44),(9,16),(47,20),(33,70),(25,69),(59,49),(51,41),(5,69),(73,4),(62,52),(37,31),(69,63),(0,72),(18,68),(29,53),(3,56),(31,25),(63,15),(38,47),(30,31),(67,48),(13,3),(60,6),(25,29),(1,13),(20,59),(40,42),(35,33),(0,43),(68,51),(51,37),(48,50),(69,40),(33,54),(70,55),(57,50),(73,73),(8,26),(37,27),(24,21),(51,48),(49,36),(34,61),(72,8)]
n::Int
n=70

adviceAgent3 :: [Coordinate] -> Int -> [Coordinate]
adviceAgent3 agents n 
    | n == 0 = []
    | null agents' = grid
    | agents' == grid = []
    | otherwise = safestPlaces
    where
        grid = everywhere n
        agents' = nub $ filter (inside n) agents
        distances = elems $ makeArray agents' n
        maxMinDistance = maximum (map snd distances)
        safestPlaces = map fst (filter ((== maxMinDistance) . snd) distances)

makeArray agents n = runSTArray $ do
    arr <- newArray ((0,0),(n-1,n-1)) ((0,0), 2*n :: Int)   -- arr[0..n-1][0..n-1]の配列を初期値((0,0),2*n)で作成
    forM_ [0..n-1] $ \y -> do
        forM_ [0..n-1] $ \x -> do
            forM_ agents $ \a -> do
                dist <- readArray arr (x,y)
                let mdis = manhattanDistance (x,y) a
                writeArray arr (x,y) ((x,y), if snd dist > mdis then mdis else snd dist)
    return arr

adviceAgent4 :: [Coordinate] -> Int -> [Coordinate]
adviceAgent4 [] _ = []
adviceAgent4 agents n
    | n == 0 = []
    | null agents' = grid
    | agents' == grid = []
    | otherwise = safestPlaces
    where
        grid = everywhere n
        agents' = nub $ filter (inside n) agents
        safestPlaces = searchPlaces grid (-1) []
        searchPlaces [] _ places = places
        searchPlaces (x:xs) maxd places = let
            mind = minDistance agents' x (2*n)
            in  if mind > maxd then searchPlaces xs mind [x] 
                else if mind == maxd then searchPlaces xs maxd (x:places) 
                else searchPlaces xs maxd places
        minDistance _ _ 0 = 0
        minDistance [] _ d = d
        minDistance (x:xs) y d = minDistance xs y $ min d $ manhattanDistance x y


{-
マップの各地点から、エージェントまでの最短距離を割り出して、
最も大きいものだけを残す、というシンプルなアルゴリズムにしてしまうと、
O(agent*n*n)となり計算量が爆発する。

こういうのはどうか。
マップからエージェントの場所を取り除く。
エージェントの１つ隣の場所を計算し、マップから取り除く。
エージェントの２つ隣の場所を計算し、マップから取り除く。
これを繰り返し、全部なくなる直前まで残っていたものが答え。
試したらこっちの方が遅かった。

最終的には、まずJavaScriptで解いて、Haskellで書き直した。
adviceAgent4が最終形。

-}

