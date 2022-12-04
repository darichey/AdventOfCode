{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2021.Day10 (solution) where

import Solution (Solution (Solution))
import Util (median)
import Data.Either (lefts, rights)

parse :: String -> Maybe [String]
parse = Just . lines

-- either the completion string or the first illegal character
complete :: String -> Either Char String
complete = go []
  where
    go stack ('(':ys) = go (')':stack) ys
    go stack ('[':ys) = go (']':stack) ys
    go stack ('{':ys) = go ('}':stack) ys
    go stack ('<':ys) = go ('>':stack) ys
    go (x:xs) (y:ys) = if x /= y then Left y else go xs ys
    go stack "" = Right stack

part1 :: [String] -> Int
part1 = sum . fmap score . lefts . fmap complete
  where
    score :: Char -> Int
    score ')' = 3
    score ']' = 57
    score '}' = 1197
    score '>' = 25137

part2 :: [String] -> Int
part2 = median . fmap score . rights . fmap complete
  where
    score :: String -> Int
    score = go 0
      where
        go s (')':rest) = go (5 * s + 1) rest
        go s (']':rest) = go (5 * s + 2) rest
        go s ('}':rest) = go (5 * s + 3) rest
        go s ('>':rest) = go (5 * s + 4) rest
        go s "" = s

solution :: Solution [String] Int Int
solution = Solution "Day 10" "input/Year2021/day10.txt" parse part1 part2
