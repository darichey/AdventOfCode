module Year2021.Day01 (solution) where

import Solution (Solution (Solution))
import Util (windows, count)

parse :: String -> Maybe [Int]
parse = Just . fmap read . lines

part1 :: [Int] -> Int
part1 = count (>0) . fmap (\[a,b] -> b - a) . windows 2

part2 :: [Int] -> Int
part2 = part1 . fmap sum . windows 3

solution :: Solution [Int] Int Int
solution = Solution "Day 1" "input/Year2021/day1.txt" parse part1 part2
