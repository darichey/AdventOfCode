module Year2022.Day06 (solution) where

import Solution (Solution (Solution))
import Data.List (findIndex, nub)
import Data.Maybe (fromJust)
import Util (windows)

parse :: String -> Maybe String
parse = Just

solve :: Eq a => Int -> [a] -> Int
solve n input = n + fromJust (findIndex (\l -> l == nub l) (windows n input))

part1 :: String -> Int
part1 = solve 4

part2 :: String -> Int
part2 = solve 14

solution :: Solution String Int Int
solution = Solution "Day 6" "input/Year2022/day6.txt" parse part1 part2
