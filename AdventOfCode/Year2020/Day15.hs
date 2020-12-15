module Year2020.Day15 where

import qualified Data.IntMap.Strict as IntMap
import Data.List.Split (splitOn)
import Solution (Solution (Solution))

parse :: String -> Maybe [Int]
parse = Just . fmap read . splitOn ","

playGame :: Int -> [Int] -> Int
playGame finalNum starting = go initialPrev (length starting + 1) initialSeen
  where
    initialSeen = IntMap.fromList $ zip (take (length starting - 1) starting) [1 ..]
    initialPrev = last starting

    go :: Int -> Int -> IntMap.IntMap Int -> Int
    go prev i seen
      | i == finalNum = next
      | otherwise = go next (i + 1) (IntMap.insert prev (i - 1) seen)
      where
        next = maybe 0 ((i - 1) - ) (IntMap.lookup prev seen)

part1 :: [Int] -> Int
part1 = playGame 2020

part2 :: [Int] -> Int
part2 = playGame 30000000

solution :: Solution [Int] Int Int
solution = Solution "Day 15" "input/Year2020/day15.txt" parse part1 part2
