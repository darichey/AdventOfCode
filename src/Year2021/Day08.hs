{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- not used because part2 isn't done
module Year2021.Day08 (solution) where

import Solution (Solution (Solution))
import Data.List.Split (splitOn)
import Util (count)
import Data.Set (Set)
import qualified Data.Set as Set

data NoteEntry = NoteEntry { patterns :: [Set Char], output :: [Set Char] }

parse :: String -> Maybe [NoteEntry]
parse = Just . fmap ((\[p, o] -> NoteEntry (Set.fromList <$> words p) (Set.fromList <$> words o)) . splitOn " | ") . lines

part1 :: [NoteEntry] -> Int
part1 = count (`elem` [2,4,3,7]) . fmap length . concatMap output

part2 :: [NoteEntry] -> Int
part2 = undefined

solution :: Solution [NoteEntry] Int Int
solution = Solution "Day 8" "input/Year2021/day8.txt" parse part1 part2
