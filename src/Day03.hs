module Day03 where

import Data.List.Split
import Data.List
import Data.Ord
import Debug.Trace
import Data.Set

data Direction = L Int | R Int | U Int | D Int deriving (Show)
type Path = [Direction]
type Point = (Int, Int)

getInput :: IO (Path, Path)
getInput = do
    fileContent <- readFile "input/day3.txt"
    let fileLines = lines fileContent
    let mapped = fmap ((fmap parseDirection) . splitOn ",") fileLines
    return (mapped !! 0, mapped !! 1)

    where
        parseDirection :: String -> Direction
        parseDirection (x:xs) = case x of
            'L' -> L d
            'R' -> R d
            'U' -> U d
            'D' -> D d
            where d = read xs

locations :: Path -> Point -> Set Point
locations [] p = singleton p
locations (d:ds) p = Data.Set.union (fromList between) (locations ds (head between))
    where between = locations' p d

locations' :: Point -> Direction -> [Point]
locations' (x,y) (L d) = fmap (\d' -> ((x - d'), y)) [d,(d-1)..1]
locations' (x,y) (R d) = fmap (\d' -> ((x + d'), y)) [d,(d-1)..1]
locations' (x,y) (U d) = fmap (\d' -> (x, (y + d'))) [d,(d-1)..1]
locations' (x,y) (D d) = fmap (\d' -> (x, (y - d'))) [d,(d-1)..1]

distance :: Point -> Point -> Int
distance (x1, y1) (x2, y2) = (abs (x1-x2)) + (abs (y1-y2))

commonPoints :: Path -> Path -> Set Point
commonPoints path1 path2 = intersection (locations path1 (0,0)) (locations path2 (0,0))

day03a :: Path -> Path -> Int
day03a path1 path2 = distance (0,0) (minimumBy (comparing (distance (0,0))) (commonPoints path1 path2))

steps :: Point -> Path -> Int
steps p path = steps' (0,0) p path

steps' :: Point -> Point -> Path -> Int
steps' p1 p2 _ | p1 == p2 = 0
steps' p1 p2 (d:ds) = 1 + (steps' (go1 p1 d) p2 newPath)
    where
        newPath = case d of
            (L 1) -> ds
            (R 1) -> ds
            (U 1) -> ds
            (D 1) -> ds
            (L x) -> (L (x-1)):ds
            (R x) -> (R (x-1)):ds
            (U x) -> (U (x-1)):ds
            (D x) -> (D (x-1)):ds

        go1 :: Point -> Direction -> Point
        go1 (x,y) (L _) = (x-1,y)
        go1 (x,y) (R _) = (x+1,y)
        go1 (x,y) (U _) = (x,y+1)
        go1 (x,y) (D _) = (x,y-1)

day03b :: Path -> Path -> Int
day03b path1 path2 = findMin . Data.Set.map (\p -> steps p path1 + steps p path2) $ commonPoints path1 path2

solutions :: IO (Int, Int)
solutions = do
    (path1, path2) <- getInput
    return (day03a path1 path2, day03b path1 path2)