module Year2015.Day05 (solution) where

import Data.List (group, isInfixOf)
import Solution (Solution (Solution))
import Util (count, windows)

parse :: String -> Maybe [String]
parse = Just . lines

part1 :: [String] -> Int
part1 = count valid
  where
    valid input = rule1 input && rule2 input && rule3 input

    rule1 input = length (filter (`elem` "aeiou") input) >= 3
    rule2 input = any ((>= 2) . length) (group input)
    rule3 input = not (or $ fmap (`isInfixOf` input) ["ab", "cd", "pq", "xy"])

part2 :: [String] -> Int
part2 = count valid
  where
    valid input = rule1 input && rule2 input

    rule1 "" = False
    rule1 (_ : "") = False
    rule1 (a : b : rest) = ((a : b : "") `isInfixOf` rest) || rule1 (b : rest)

    rule2 input = any (\[a, _, c] -> a == c) (windows 3 input)

solution :: Solution [String] Int Int
solution = Solution "Day 5" "input/Year2015/day5.txt" parse part1 part2
