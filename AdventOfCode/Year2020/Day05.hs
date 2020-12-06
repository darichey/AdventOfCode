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
part2 = IntSet.findMin . (all >>= IntSet.difference)
  where
    all seats = IntSet.fromList [IntSet.findMin seats .. IntSet.findMax seats]

solution :: Solution IntSet.IntSet Int Int
solution = Solution "Day 5" "input/Year2020/day5.txt" parse part1 part2
