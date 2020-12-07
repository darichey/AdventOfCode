module Year2015.Day03 (solution) where

import Data.List (nub)
import Solution (Solution (Solution))

type Pos = (Int, Int)

parse :: String -> Maybe String
parse = Just

move :: Pos -> Char -> Pos
move (x, y) '^' = (x, y + 1)
move (x, y) '>' = (x + 1, y)
move (x, y) 'v' = (x, y -1)
move (x, y) '<' = (x -1, y)

part1 :: String -> Int
part1 = length . nub . scanl move (0, 0)

part2 :: String -> Int
part2 = length . nub . bothPositions . scanl moveEveryOther ((0, 0), (0, 0)) . zip [0 ..]
  where
    moveEveryOther :: (Pos, Pos) -> (Int, Char) -> (Pos, Pos)
    moveEveryOther (santa, robot) (i, c) =
      if even i
        then (move santa c, robot)
        else (santa, move robot c)

    bothPositions :: [(Pos, Pos)] -> [Pos]
    bothPositions list = list >>= (\(p1, p2) -> [p1, p2])

solution :: Solution String Int Int
solution = Solution "Day 3" "input/Year2015/day3.txt" parse part1 part2
