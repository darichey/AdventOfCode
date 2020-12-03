module Year2019.Day01 (solution) where

import Solution (Solution (Solution))

parse :: String -> Maybe [Int]
parse = Just . fmap read . lines

-- | Claculates the simple amount of fuel required for a module
fuel :: Int -> Int
fuel mass = max ((mass `div` 3) - 2) 0

-- | Calculates the total amount of fuel required for a module, taking into
--  account the fact that fuel itself requires more fuel
allFuel :: Int -> Int
allFuel 0 = 0
allFuel x = y + allFuel y
  where
    y = fuel x

part1 :: [Int] -> Int
part1 = sum . fmap fuel

part2 :: [Int] -> Int
part2 = sum . fmap allFuel

solution :: Solution [Int] Int Int
solution = Solution "Day 1" "input/Year2019/day1.txt" parse part1 part2
