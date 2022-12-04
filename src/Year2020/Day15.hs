module Year2020.Day15 (solution) where

import qualified Data.IntMap as IntMap
import Data.List.Split (splitOn)
import Solution (Solution (Solution))

parse :: String -> Maybe [Int]
parse = Just . fmap read . splitOn ","

playGame :: Int -> [Int] -> Int
playGame numTurns starting = go initialPrev (length starting + 1) initialSeen
  where
    initialSeen = IntMap.fromList $ zip (init starting) [1 ..]
    initialPrev = last starting

    go :: Int -> Int -> IntMap.IntMap Int -> Int
    go prev turn seen
      | turn == numTurns = next
      | otherwise = go next (turn + 1) (IntMap.insert prev (turn - 1) seen)
      where
        next = maybe 0 ((turn - 1) -) (IntMap.lookup prev seen)

part1 :: [Int] -> Int
part1 = playGame 2020

part2 :: [Int] -> Int
part2 = playGame 30000000

solution :: Solution [Int] Int Int
solution = Solution "Day 15" "input/Year2020/day15.txt" parse part1 part2
