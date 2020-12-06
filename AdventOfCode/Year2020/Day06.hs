module Year2020.Day06 where

import Data.List (foldl1')
import Data.List.Split (splitOn)
import qualified Data.Set as Set
import Solution (Solution (Solution))

parse :: String -> Maybe [[Set.Set Char]]
parse = Just . fmap (fmap Set.fromList . splitOn "\n") . splitOn "\n\n"

part1 :: [[Set.Set Char]] -> Int
part1 = sum . fmap (Set.size . foldl1' Set.union)

part2 :: [[Set.Set Char]] -> Int
part2 = sum . fmap (Set.size . foldl1' Set.intersection)

solution :: Solution [[Set.Set Char]] Int Int
solution = Solution "Day 6" "input/Year2020/day6.txt" parse part1 part2