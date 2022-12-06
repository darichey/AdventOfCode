module Year2022.Day03 (solution) where

import Solution (Solution (Solution))
import qualified Data.Set as Set
import Data.Char (isLower, ord)
import Data.List.Split (chunksOf)
import Data.List (foldl1')

commonItem :: [String] -> Char
commonItem = head . Set.toList . foldl1' Set.intersection . fmap Set.fromList

priority :: Char -> Int
priority item = if isLower item then ord item - 96 else ord item - 38

parse :: String -> Maybe [String]
parse = Just . lines

part1 :: [String] -> Int
part1 = sum . fmap (priority . commonItem . (\(fst, snd) -> [fst, snd]) . (\line -> splitAt (length line `div` 2) line))

part2 :: [String] -> Int
part2 = sum . fmap (priority . commonItem) . chunksOf 3

solution :: Solution [String] Int Int
solution = Solution "Day 3" "input/Year2022/day3.txt" parse part1 part2
