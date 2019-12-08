-- https://adventofcode.com/2019/day/2
module Day02 (solutions) where

import Data.List.Split
import Data.Vector

type Program = Vector Int

-- |Reads the input IntCode program as a Program
getInput :: IO Program
getInput = fromList . fmap read . splitOn "," <$> readFile "input/day2.txt"

-- |Runs an IntCode program given a noun and a verb. The result is the value
-- at position 0 after the program completes.
solve :: Int -> Int -> Program -> Int
solve noun verb prog = Data.Vector.head (solve' 0 (fix prog))
    where
        -- |Runs an IntCode program starting at the given pointer index
        solve' :: Int -> Program -> Program
        solve' ptr prog =
            case op of
                1 -> solve' (ptr + 4) (prog // [(z, x + y)])
                2 -> solve' (ptr + 4) (prog // [(z, x * y)])
                99 -> prog
            where
                op = prog ! ptr
                x  = prog ! (prog ! (ptr + 1))
                y  = prog ! (prog ! (ptr + 2))
                z  = prog ! (ptr + 3)

        -- |Replaces the values at positions 1 and 2 with the noun and verb
        fix :: Program -> Program
        fix prog = prog // [(1, noun), (2, verb)]

-- |Solves part one by running the IntCode program with noun=12 and verb=2
day02a :: Program -> Int
day02a = solve 12 2

-- |Solves part two by running the program first with noun=0,verb=0 and then
-- with noun=1,verb=0. We realize that the result of the program has a linear
-- relationship with the noun and verb. Specifically,
-- res = base + noun*delta + base. We find the solution for the desired res
day02b :: Program -> Int
day02b prog = noun * 100 + verb
    where
        base  = solve 0 0 prog
        delta = solve 1 0 prog - base
        (noun, verb) = divMod (19690720 - base) delta

-- Bruteforce Solution for part two
-- day02b :: Program -> Int
-- day02b prog = noun * 100 + verb
--     where (noun, verb) = Prelude.head [(noun,verb) | noun <- [0..99],
--                                                      verb <- [0..99],
--                                                      solve noun verb prog == 19690720]

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day02a input, day02b input)