module Year2020.Day11 (solution) where

import Data.Foldable (find)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe)
import Solution (Solution (Solution))
import Util (occurrences)

type Point = (Int, Int)

type Tile = Char

type Grid = Map.Map Point Tile

parse :: String -> Maybe Grid
parse = Just . Map.fromList . withIndices . lines
  where
    withIndices :: [[a]] -> [(Point, a)]
    withIndices list = zip [0 ..] (fmap (zip [0 ..]) list) >>= (\(y, xs) -> fmap (\(x, t) -> ((x, y), t)) xs)

gridMapWithNeighbors :: (Point -> [Point]) -> ([Tile] -> Tile -> Tile) -> Grid -> Grid
gridMapWithNeighbors neighbors step grid = Map.mapWithKey (step . mapMaybe (`Map.lookup` grid) . neighbors) grid

newTile :: Int -> [Tile] -> Tile -> Tile
newTile _ _ '.' = '.'
newTile _ around 'L' = if '#' `notElem` around then '#' else 'L'
newTile leaveThreshold around '#' = if occurrences '#' around >= leaveThreshold then 'L' else '#'

stable :: (Eq a) => (a -> a) -> a -> a
stable step initial = fst $ fromJust $ find (uncurry (==)) (pairs $ iterate step initial)
  where
    pairs :: [a] -> [(a, a)]
    pairs xs = zip xs (tail xs)

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

adjacentOffsets :: [Point]
adjacentOffsets = [(-1, 1), (0, 1), (1, 1), (-1, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]

part1 :: Grid -> Int
part1 grid = occurrences '#' finalGrid
  where
    finalGrid = stable (gridMapWithNeighbors adjacentPoints (newTile 4)) grid
    adjacentPoints p = fmap (addPoints p) adjacentOffsets

part2 :: Grid -> Int
part2 grid = occurrences '#' finalGrid
  where
    finalGrid = stable (gridMapWithNeighbors nearestChairs (newTile 5)) grid

    nearestChairs :: Point -> [Point]
    nearestChairs p = fmap (\offset -> until isntFloor (addPoints offset) (addPoints p offset)) adjacentOffsets

    isntFloor :: Point -> Bool
    isntFloor p = Map.lookup p grid /= Just '.'

solution :: Solution Grid Int Int
solution = Solution "Day 11" "input/Year2020/day11.txt" parse part1 part2
