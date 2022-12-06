{-# LANGUAGE OverloadedStrings #-}

module Year2022.Day05 (solution) where

import Data.Foldable (toList)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Debug.Trace (traceShowId)
import Solution (Solution (Solution))

data Input = Input (Seq [Char]) [(Int, Int, Int)]

parse :: String -> Maybe Input
parse = Just . (\[stacks, moves] -> Input (parseStacks stacks) (parseMoves moves)) . splitOn "\n\n"
  where
    parseStacks = Seq.fromList . filter (/= "") . fmap (filter (/= ' ')) . transpose . init . splitOn "\n" . Text.unpack . Text.replace "   " " " . Text.replace "]" "" . Text.replace "[" "" . Text.pack

    parseMoves = fmap ((\["move", amt, "from", from, "to", to] -> (read amt, read from, read to)) . splitOn " ") . lines

part1 :: Input -> String
part1 (Input stacks moves) = go moves stacks
  where
    go ((0, _, _):moves) stacks = go moves stacks
    go ((amt, from, to) : moves) stacks = go ((amt - 1, from, to) : moves) (Seq.adjust' (\(_ : stack) -> stack) (from - 1) (Seq.adjust' (\stack -> head (Seq.index stacks (from - 1)) : stack) (to - 1) stacks))
    go [] stacks = fmap head $ toList stacks

part2 :: Input -> String
part2 (Input stacks moves) = go moves stacks
  where
    go ((amt, from, to) : moves) stacks = go moves (Seq.adjust' (drop amt) (from - 1) (Seq.adjust' (\stack -> take amt (Seq.index stacks (from - 1)) ++ stack) (to - 1) stacks))
    go [] stacks = fmap head $ toList stacks

solution :: Solution Input String String
solution = Solution "Day 5" "input/Year2022/day5.txt" parse part1 part2
