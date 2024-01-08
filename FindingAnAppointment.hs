-- https://www.codewars.com/kata/525f277c7103571f47000147
-- Finding an appointment

module FindingAnAppointment where

import Data.List

getStartTime :: [[(String, String)]] -> Int -> Maybe String
getStartTime schedules duration = let
    all = groupSchedules $ margeSchedules $ schedules ++ [[("00:00","09:00"),("19:00","24:00")]]
    rslt = searchDuration all duration
    in if rslt == [] then Nothing else Just (head rslt)

timeToMinute :: String -> Int
timeToMinute [h1,h0,_,m1,m0] = read [h1,h0] * 60 + read [m1,m0]

minuteToTime :: Int -> String
minuteToTime m = zeroPadding (m `div` 60) ++ ":" ++ zeroPadding (m `mod` 60)
    where zeroPadding v = (if v < 10 then "0" else "") ++ show v

scheduleToZone :: (String, String) -> [Int]
scheduleToZone (from,to) = [timeToMinute from .. timeToMinute to - 1]

zoneToSchedule :: [Int] -> (String, String)
zoneToSchedule zone = (minuteToTime (head zone), minuteToTime (last zone + 1))

margeSchedules :: [[(String, String)]] -> [Int]
margeSchedules schedules = sort $ nub $  concat $ map scheduleToZone $ concat schedules

groupSchedules :: [Int] -> [(String, String)]
groupSchedules zones = map zoneToSchedule $ groupBy (\a b -> a + 1 == b) zones
    where
        groupBy :: (a -> a -> Bool) -> [a] -> [[a]] -- Data.List.GroupBy
        groupBy _ [] = []
        groupBy p' (x':xs') = (x' : ys') : zs'
            where
                (ys',zs') = go p' x' xs'
                go p z (x:xs)
                    | p z x = (x : ys, zs)
                    | otherwise = ([], (x : ys) : zs)
                    where (ys,zs) = go p x xs
                go _ _ [] = ([], [])

searchDuration :: [(String, String)] -> Int -> [String]
searchDuration [] _ = []
searchDuration [x] _ = []
searchDuration (x@(_,to) : y@(from,_) :zs) d =
    (if timeToMinute from - timeToMinute to >= d then [to] else []) ++ searchDuration (y:zs) d

schedules =
            [ [("09:00", "11:30"), ("13:30", "16:00"), ("16:00", "17:30"), ("17:45", "19:00")]
            , [("09:15", "12:00"), ("14:00", "16:30"), ("17:00", "17:30")]
            , [("11:30", "12:15"), ("15:00", "16:30"), ("17:45", "19:00")]
            ]
