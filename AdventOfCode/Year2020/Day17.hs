module Year2020.Day17 (solution) where

import qualified Data.Set as Set
import Linear (V4 (V4))
import Solution (Solution (Solution))

type Point = V4 Int

type Grid = Set.Set Point

parse :: String -> Maybe Grid
parse input = Just $ Set.fromList activePoints
  where
    rows = zip [0 ..] (lines input)

    allPoints :: [(Point, Char)]
    allPoints = rows >>= (\(y, row) -> fmap (\(x, c) -> (V4 x y 0 0, c)) (zip [0 ..] row))

    activePoints :: [Point]
    activePoints = fmap fst (filter ((==) '#' . snd) allPoints)

step :: (Point -> Set.Set Point) -> Grid -> Grid
step adjacent active = Set.filter shouldBeActive (foldMap adjacent active)
  where
    shouldBeActive :: Point -> Bool
    shouldBeActive p
      | p `elem` active = activeNeighbors == 2 || activeNeighbors == 3
      | otherwise = activeNeighbors == 3
      where
        activeNeighbors = Set.size $ adjacent p `Set.intersection` active

part1 :: Grid -> Int
part1 = length . (!! 6) . iterate (step adjacent3)
  where
    adjacent3 :: Point -> Set.Set Point
    adjacent3 point = Set.delete point (Set.fromList (fmap (+ point) [V4 x y z 0 | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1]]))

part2 :: Grid -> Int
part2 = length . (!! 6) . iterate (step adjacent4)
  where
    adjacent4 :: Point -> Set.Set Point
    adjacent4 point = Set.delete point (Set.fromList (fmap (+ point) [V4 x y z w | x <- [-1 .. 1], y <- [-1 .. 1], z <- [-1 .. 1], w <- [-1 .. 1]]))

solution :: Solution Grid Int Int
solution = Solution "Day 17" "input/Year2020/day17.txt" parse part1 part2
