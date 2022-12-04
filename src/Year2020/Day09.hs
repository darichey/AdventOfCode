module Year2020.Day09 (solution) where

import Data.List (find)
import Data.Maybe (fromJust, isNothing)
import Solution (Solution (Solution))
import Util (twoSum, windows)

parse :: String -> Maybe [Int]
parse = Just . fmap read . lines

part1 :: [Int] -> Int
part1 = head . fromJust . find cantMake . windows 26 . reverse
  where
    cantMake :: [Int] -> Bool
    cantMake (target : ints) = isNothing $ twoSum target ints

part2 :: [Int] -> Int
part2 ints = minimum range + maximum range
  where
    range = fromJust $ find ((== part1 ints) . sum) subLists
    subLists = [2 .. length ints] >>= (`windows` ints)

-- O(n) solution but... worse? need to experiment
-- range = go 0 0 0

-- target = part1 ints
-- vec = Vector.fromList ints

-- go :: Int -> Int -> Int -> [Int]
-- go start end sum
--   | sum == target = Vector.toList $ Vector.slice start (end - start + 1) vec
--   | sum > target = go (start + 1) end (sum - (vec Vector.! start))
--   | sum < target = go start (end + 1) (sum + (vec Vector.! end))

solution :: Solution [Int] Int Int
solution = Solution "Day 9" "input/Year2020/day9.txt" parse part1 part2
