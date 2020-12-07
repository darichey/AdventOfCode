module Year2015.Day01 (solution) where

import Solution (Solution(Solution))
import Data.List (elemIndex)
import Data.Maybe (fromJust)

parse :: String -> Maybe [Int]
parse = Just . scanl (\acc c -> if c == '(' then acc + 1 else acc - 1) 0

part1 :: [Int] -> Int
part1 = last

part2 :: [Int] -> Int
part2 = fromJust . elemIndex (-1)

solution :: Solution [Int] Int Int
solution = Solution "Day 01" "input/Year2015/day1.txt" parse part1 part2
