module Year2020.Day10 (solution) where

import Solution (Solution(Solution))
import Data.List (sort)
import Util (occurrences, windows)
import qualified Data.IntMap as IntMap

parse :: String -> Maybe [Int]
parse = Just . fmap read . lines

prepare :: [Int] -> [Int]
prepare ints = sort (0 : m : ints)
  where
    m = maximum ints + 3

part1 :: [Int] -> Int
part1 ints = occurrences 1 diffs * occurrences 3 diffs
  where
    diffs = (\[x,y] -> y - x) <$> windows 2 (prepare ints)

part2 :: [Int] -> Int
part2 = snd . IntMap.findMax . allArrangements . tail . prepare
  where
    allArrangements :: [Int] -> IntMap.IntMap Int
    allArrangements = foldl (\map x -> IntMap.insert x (arrangements map x) map) (IntMap.singleton 0 1)
    
    arrangements :: IntMap.IntMap Int -> Int -> Int
    arrangements dp x = sum $ (\y -> IntMap.findWithDefault 0 y dp) <$> [x - 1, x - 2, x - 3]


solution :: Solution [Int] Int Int
solution = Solution "Day 10" "input/Year2020/day10.txt" parse part1 part2
