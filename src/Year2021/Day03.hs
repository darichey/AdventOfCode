module Year2021.Day03 (solution) where

import Solution (Solution (Solution))
import Util (occurrences)
import Data.List (transpose, foldl')
import Data.Char (digitToInt)

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

flipBits :: String -> String
flipBits ('1':s) = '0' : flipBits s
flipBits ('0':s) = '1' : flipBits s
flipBits "" = ""

mostCommon :: String -> Char
mostCommon = mostCommon' '1'

mostCommon' :: Char -> String -> Char
mostCommon' tieBreaker s = case compare (fromIntegral $ occurrences '1' s) (fromIntegral (length s) / 2) of
  LT -> '0'
  EQ -> tieBreaker
  GT -> '1'

leastCommon' :: Char -> String -> Char
leastCommon' tieBreaker s = case compare (fromIntegral $ occurrences '1' s) (fromIntegral (length s) / 2) of
  LT -> '1'
  EQ -> tieBreaker
  GT -> '0'


parse :: String -> Maybe [String]
parse = Just . lines

part1 :: [String] -> Int
part1 nums = gamma * epsilon
  where
    gammaString = mostCommon <$> transpose nums
    gamma = toDec gammaString
    epsilon =  toDec $ flipBits gammaString

part2 :: [String] -> Int
part2 nums = oxygen * co2
  where
    -- gammaString = traceShowId $ 
    -- epsilonString = flipBits gammaString

    oxygen = toDec $ go nums 0

    go [final] _ = final
    go remaining i = go (filter (\s -> s !! i == (mostCommon' '1' <$> transpose remaining) !! i) remaining) (i + 1)

    co2 = toDec $ go2 nums 0

    go2 [final] _ = final
    go2 remaining i = go2 (filter (\s -> s !! i == (leastCommon' '0' <$> transpose remaining) !! i) remaining) (i + 1)

solution :: Solution [String] Int Int
solution = Solution "Day 3" "input/Year2021/day3.txt" parse part1 part2
