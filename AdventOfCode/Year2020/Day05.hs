module Year2020.Day05 (solution) where

import qualified Data.IntSet as IntSet
import Data.Char (digitToInt)
import Data.List (foldl')
import Solution (Solution (Solution))

seatToBin :: String -> String
seatToBin = fmap replace
  where
    replace 'B' = '1'
    replace 'F' = '0'
    replace 'R' = '1'
    replace 'L' = '0'
    replace x = x

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parse :: String -> Maybe IntSet.IntSet
parse = Just . IntSet.fromList . (parseSeat . seatToBin <$>) . lines
  where
    parseSeat str = (toDec $ take 7 str) * 8 + (toDec $ drop 7 str)

part1 :: IntSet.IntSet -> Int
part1 = IntSet.findMax

part2 :: IntSet.IntSet -> Int
part2 seats = IntSet.findMin missing + 1
  where
    matching = IntSet.filter (\k -> IntSet.member (k + 1) seats) seats
    missing = seats IntSet.\\ matching

solution :: Solution IntSet.IntSet Int Int
solution = Solution "Day 5" "input/Year2020/day5.txt" parse part1 part2