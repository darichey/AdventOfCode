module Year2021.Day07 (solution) where

import Solution (Solution (Solution))
import Data.List (sort)
import Data.List.Split (splitOn)

parse :: String -> Maybe [Int]
parse = Just . fmap read . splitOn ","

part1 :: [Int] -> Int
part1 nums = costToMove (median nums) nums
  where
    median xs = sort xs !! (length xs `div` 2)
    costToMove to = sum . fmap (abs . (to -))

part2 :: [Int] -> Int
part2 nums = min (costToMove (floor $ average nums) nums) (costToMove (ceiling $ average nums) nums)
  where
    costToMove to = sum . fmap (triangular . abs . (to -))
    triangular n = n * (n + 1) `div` 2
    average xs = fromIntegral (sum xs) / fromIntegral (length xs)

solution :: Solution [Int] Int Int
solution = Solution "Day 7" "input/Year2021/day7.txt" parse part1 part2
