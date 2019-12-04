module Day03 where

import Data.List.Split
import Data.Ord
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

data Direction = L | R | U | D deriving (Show)
type Cmd = (Direction, Int)
type Path = [Cmd]
type Point = (Int, Int)

getInput :: IO (Path, Path)
getInput = (firstTwo . (fmap parsePath) . lines) <$> readFile "input/day3.txt"
    where
        firstTwo (x:y:_) = (x,y)
        parsePath = parseDirections . (splitOn ",")
        parseDirections = fmap parseDirection
        parseDirection (d:is) = case d of
            'L' -> (L, i)
            'R' -> (R, i)
            'U' -> (U, i)
            'D' -> (D, i)
            where i = read is

points :: Path -> M.Map Point Int
points = points' 0 (0,0)

points' :: Int -> Point -> Path -> M.Map Point Int
points' steps start [] = M.singleton start steps
points' steps (x,y) ((dir,dist):path) = M.union (M.fromList $ between) (points' (steps+dist) ((fst . head) between) path)
    where
        f :: Direction -> Int -> (Point, Int)
        f dir dist' = case dir of
            L -> ((x-dist', y), steps+dist')
            R -> ((x+dist', y), steps+dist')
            U -> ((x, y+dist'), steps+dist')
            D -> ((x, y-dist'), steps+dist')
        
        between :: [(Point, Int)]
        between = (f dir) <$> [dist,dist-1 .. 1]

distanceZero :: Point -> Int
distanceZero (x1, y1) = (abs x1) + (abs y1)

pathIntersections :: Path -> Path -> M.Map Point Int
pathIntersections path1 path2 = M.intersectionWith (+) (points path1) (points path2)

day03a :: Path -> Path -> Int
day03a path1 path2 = distanceZero closestPoint
    where closestPoint = minimumBy (comparing distanceZero) (M.keysSet $ pathIntersections path1 path2)

day03b :: Path -> Path -> Int
day03b path1 path2 = minimum $ M.elems (pathIntersections path1 path2)

solutions :: IO (Int, Int)
solutions = do
    (path1, path2) <- getInput
    return (day03a path1 path2, day03b path1 path2)