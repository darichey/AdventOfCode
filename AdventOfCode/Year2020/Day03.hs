{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Year2020.Day03 (solution) where

import Solution (Solution (Solution))
import Util (occurrences)

data Map = Map {rows :: Int, cols :: Int, grid :: [[Char]]}

data Slope = Slope {dx :: Int, dy :: Int}

data Pos = Pos {x :: Int, y :: Int}

parse :: String -> Maybe Map
parse input =
  let grid = lines input
   in Just $ Map (length grid) (length $ head grid) grid

get :: Map -> Pos -> Char
get Map {grid} Pos {..} = (grid !! y) !! x

onMap :: Map -> Pos -> Bool
onMap Map {rows} Pos {y} = y < rows

step :: Map -> Slope -> Pos -> Pos
step Map {..} Slope {..} Pos {..} = Pos ((x + dx) `mod` cols) (y + dy)

countTrees :: Map -> Slope -> Int
countTrees m@Map {..} s@Slope {..} = occurrences '#' $ get m <$> visited
  where
    visited = takeWhile (onMap m) (iterate (step m s) (Pos 0 0))

part1 :: Map -> Int
part1 map = countTrees map (Slope 3 1)

part2 :: Map -> Int
part2 map = product $ fmap (countTrees map . uncurry Slope) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

solution :: Solution Map Int Int
solution = Solution "Day 3" "input/Year2020/day3.txt" parse part1 part2
