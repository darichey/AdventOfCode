module Day03 (solutions) where

import Data.List.Split
import Data.Ord
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

type Direction = Char
type Path = [Direction]
type Point = (Int, Int)

getInput :: IO (Path, Path)
getInput = (firstTwo . fmap parsePath . lines) <$> readFile "input/day3.txt"
    where
        firstTwo (x:y:_) = (x,y)
        parsePath = (parseDirection =<<) . splitOn ","
        parseDirection (d:is) = replicate (read is) d

points :: Path -> M.Map Point Int
points path = M.fromListWith min (tail $ zip (scanl go (0,0) path) [0..])
    where
        go :: Point -> Direction -> Point
        go (x,y) dir = case dir of
            'L' -> (x - 1, y)
            'R' -> (x + 1, y)
            'U' -> (x, y + 1)
            'D' -> (x, y - 1)

distanceZero :: Point -> Int
distanceZero (x1, y1) = (abs x1) + (abs y1)

intersections :: Path -> Path -> M.Map Point Int
intersections path1 path2 = M.intersectionWith (+) (points path1) (points path2)

day03a :: Path -> Path -> Int
day03a path1 path2 = minimum $ S.map distanceZero (M.keysSet $ intersections path1 path2)

day03b :: Path -> Path -> Int
day03b path1 path2 = minimum $ M.elems (intersections path1 path2)

solutions :: IO (Int, Int)
solutions = do
    (path1, path2) <- getInput
    return (day03a path1 path2, day03b path1 path2)