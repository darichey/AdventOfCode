module Year2022.Day04 (solution) where

import Solution (Solution (Solution))
import Data.List.Split (splitOn)
import Util (count)

parse :: String -> Maybe [((Int, Int), (Int, Int))]
parse = Just . fmap ((\[fst, snd] -> (pair fst, pair snd)) . splitOn ",") . lines
  where
    pair range = let [start, end] = splitOn "-" range in (read start, read end)

part1 :: [((Int, Int), (Int, Int))] -> Int
part1 = count (\(range1, range2) -> contains range1 range2 || contains range2 range1)
  where
    contains (l1, r1) (l2, r2) = l2 >= l1 && l2 <= r1 && r2 >= l1 && r2 <= r1

part2 :: [((Int, Int), (Int, Int))] -> Int
part2 = count (\(range1, range2) -> overlaps range1 range2 || overlaps range2 range1)
  where
    overlaps (l1, r1) (l2, r2) = l2 >= l1 && l2 <= r1 || r2 >= l1 && r2 <= r1

solution :: Solution [((Int, Int), (Int, Int))] Int Int
solution = Solution "Day 4" "input/Year2022/day4.txt" parse part1 part2
