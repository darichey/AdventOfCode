module Year2022.Day01 (solution) where

import Solution (Solution (Solution))
import Data.List.Split (splitOn)
import Data.List (sort)

parse :: String -> Maybe [[Int]]
parse = Just . fmap (fmap read . lines) . splitOn "\n\n"

part1 :: [[Int]] -> Int
part1 = maximum . fmap sum

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . fmap sum

solution :: Solution [[Int]] Int Int
solution = Solution "Day 1" "input/Year2022/day1.txt" parse part1 part2
