module Year2019.Day08 (solution) where

import Data.List (minimumBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)
import Solution (Solution (Solution))
import Util (NoQuotes (NoQuotes), occurrences)

type Pixel = Char

type Layer = [Pixel]

type Image = [Layer]

parse :: String -> Maybe Image
parse = Just . chunksOf 150

part1 :: Image -> Int
part1 layers = ones * twos
  where
    leastZeros = minimumBy (comparing (occurrences '0')) layers
    ones = occurrences '1' leastZeros
    twos = occurrences '2' leastZeros

part2 :: Image -> String
part2 layers = unlines $ chunksOf 25 (fmap (pixelToChar . top layers) [0 .. 149])

top :: Image -> Int -> Pixel
top layers pixel = head $ filter (/= '2') (fmap (!! pixel) layers)

pixelToChar :: Pixel -> Char
pixelToChar '1' = '#'
pixelToChar _ = ' '

solution :: Solution Image Int NoQuotes
solution = Solution "Day 8" "input/Year2019/day8.txt" parse part1 (NoQuotes . part2)
