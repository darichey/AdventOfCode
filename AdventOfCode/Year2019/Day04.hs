{-# LANGUAGE TypeApplications #-}

module Year2019.Day04 (solution) where

import Data.Ix (range)
import Data.List (group)
import Data.List.Split (splitOn)
import Solution (Solution (Solution))

parse :: String -> Maybe [String]
parse = Just . fmap show . range . firstTwo . fmap (read @Int) . splitOn "-"
  where
    firstTwo [x, y] = (x, y)

pairs :: [a] -> [(a, a)]
pairs x = zip x (tail x)

twoAdjacent :: String -> Bool
twoAdjacent = any (uncurry (==)) . pairs

exactlyPair :: String -> Bool
exactlyPair = any (\x -> length x == 2) . group

increasing :: String -> Bool
increasing = all (uncurry (<=)) . pairs

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both f g x = f x && g x

part1 :: [String] -> Int
part1 = length . filter (both increasing twoAdjacent)

part2 :: [String] -> Int
part2 = length . filter (both increasing exactlyPair)

solution :: Solution [String] Int Int
solution = Solution "Day 4" "input/Year2019/day4.txt" parse part1 part2
