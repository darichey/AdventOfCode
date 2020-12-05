module Year2020.Day05 (solution) where

import qualified Data.IntSet as IntSet
import Data.List (foldl')
import Solution (Solution (Solution))

seatId :: String -> Int
seatId = foldl' (\acc x -> acc * 2 + if x == 'B' || x == 'R' then 1 else 0) 0

parse :: String -> Maybe IntSet.IntSet
parse = Just . IntSet.fromList . (seatId <$>) . lines

part1 :: IntSet.IntSet -> Int
part1 = IntSet.findMax

part2 :: IntSet.IntSet -> Int
part2 seats = IntSet.findMin missing + 1
  where
    matching = IntSet.filter (\k -> IntSet.member (k + 1) seats) seats
    missing = seats IntSet.\\ matching

solution :: Solution IntSet.IntSet Int Int
solution = Solution "Day 5" "input/Year2020/day5.txt" parse part1 part2