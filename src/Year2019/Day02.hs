-- https://adventofcode.com/2019/day/2
module Year2019.Day02 (solution) where

import Data.List.Split (splitOn)
import Data.Vector (Vector, fromList, head, (!), (//))
import Solution (Solution (Solution))

type Program = Vector Int

parse :: String -> Maybe Program
parse = Just . fromList . fmap read . splitOn ","

-- | Runs an IntCode program given a noun and a verb. The result is the value
--  at position 0 after the program completes.
solve :: Int -> Int -> Program -> Int
solve noun verb prog = Data.Vector.head (solve' 0 (fix prog))
  where
    solve' :: Int -> Program -> Program
    solve' ptr prog =
      case op of
        1 -> solve' (ptr + 4) (prog // [(z, x + y)])
        2 -> solve' (ptr + 4) (prog // [(z, x * y)])
        99 -> prog
      where
        op = prog ! ptr
        x = prog ! (prog ! (ptr + 1))
        y = prog ! (prog ! (ptr + 2))
        z = prog ! (ptr + 3)
    fix :: Program -> Program
    fix prog = prog // [(1, noun), (2, verb)]

-- | Solves part one by running the IntCode program with noun=12 and verb=2
part1 :: Program -> Int
part1 = solve 12 2

-- | Solves part two by running the program first with noun=0,verb=0 and then
--  with noun=1,verb=0. We realize that the result of the program has a linear
--  relationship with the noun and verb. Specifically,
--  res = base + noun*delta + base. We find the solution for the desired res
part2 :: Program -> Int
part2 prog = noun * 100 + verb
  where
    base = solve 0 0 prog
    delta = solve 1 0 prog - base
    (noun, verb) = divMod (19690720 - base) delta

-- Bruteforce Solution for part two
-- day02b :: Program -> Int
-- day02b prog = noun * 100 + verb
--     where (noun, verb) = Prelude.head [(noun,verb) | noun <- [0..99],
--                                                      verb <- [0..99],
--                                                      solve noun verb prog == 19690720]

solution :: Solution Program Int Int
solution = Solution "Day 2" "input/Year2019/day2.txt" parse part1 part2
