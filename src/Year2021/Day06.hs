module Year2021.Day06 (solution) where

import Solution (Solution (Solution))
import Data.List.Split (splitOn)
import Util (occurrences)

parse :: String -> Maybe [Int]
parse input = Just $ fmap (`occurrences` nums) [0..8]
  where
    nums = read <$> splitOn "," input

simulate :: [Int] -> [Int]
simulate [d0,d1,d2,d3,d4,d5,d6,d7,d8] = [d1,d2,d3,d4,d5,d6,d7+d0,d8,d0]

solve :: Int -> [Int] -> Int
solve days = sum . (!! days) . iterate simulate

part1 :: [Int] -> Int
part1 = solve 80

part2 :: [Int] -> Int
part2 = solve 256

solution :: Solution [Int] Int Int
solution = Solution "Day 6" "input/Year2021/day6.txt" parse part1 part2
