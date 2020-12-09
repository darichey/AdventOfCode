module Year2020.Day01 (solution) where

import qualified Data.IntSet as IntSet
import Solution (Solution (Solution))
import Util (twoSum)
import Data.Maybe (fromJust)

parse :: String -> Maybe [Int]
parse = Just . fmap read . lines

part1 :: [Int] -> Int
part1 = uncurry (*) . fromJust . twoSum 2020

part2 :: [Int] -> Int
part2 ints = head [x * y * target x y | x <- ints, y <- ints, IntSet.member (target x y) set]
  where
    set = IntSet.fromList ints
    target x y = 2020 - (x + y)

solution :: Solution [Int] Int Int
solution = Solution "Day 1" "input/Year2020/day1.txt" parse part1 part2
