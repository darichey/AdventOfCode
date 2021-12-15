module Year2021.Day09 (solution) where

import Solution (Solution (Solution))
import Util (convolution)
import Data.Maybe (catMaybes)
import Data.Char (digitToInt)

parse :: String -> Maybe [[Int]]
parse = Just . fmap (fmap digitToInt) . lines

part1 :: [[Int]] -> Int
part1 = sum . catMaybes . concat . convolution (\x ns -> if all (x <) ns then Just (x + 1) else Nothing)

part2 :: [[Int]] -> Int
part2 = undefined

solution :: Solution [[Int]] Int Int
solution = Solution "Day 9" "input/Year2021/day9.txt" parse part1 part2
