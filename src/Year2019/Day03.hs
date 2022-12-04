module Year2019.Day03 (solution) where

import Control.Monad ((<=<))
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Solution (Solution (Solution))

type Direction = Char

type Path = [Direction]

type Point = (Int, Int)

parse :: String -> Maybe (Path, Path)
parse = Just . firstTwo . fmap parsePath . lines
  where
    firstTwo (x : y : _) = (x, y)
    parsePath = parseDirection <=< splitOn ","
    parseDirection (d : is) = replicate (read is) d

points :: Path -> M.Map Point Int
points path = M.fromListWith min (tail $ zip (scanl go (0, 0) path) [0 ..])
  where
    go :: Point -> Direction -> Point
    go (x, y) dir = case dir of
      'L' -> (x - 1, y)
      'R' -> (x + 1, y)
      'U' -> (x, y + 1)
      'D' -> (x, y - 1)

distanceZero :: Point -> Int
distanceZero (x1, y1) = abs x1 + abs y1

intersections :: Path -> Path -> M.Map Point Int
intersections path1 path2 = M.intersectionWith (+) (points path1) (points path2)

part1 :: (Path, Path) -> Int
part1 (path1, path2) = minimum $ S.map distanceZero (M.keysSet $ intersections path1 path2)

part2 :: (Path, Path) -> Int
part2 (path1, path2) = minimum $ M.elems (intersections path1 path2)

solution :: Solution (Path, Path) Int Int
solution = Solution "Day 3" "input/Year2019/day3.txt" parse part1 part2
