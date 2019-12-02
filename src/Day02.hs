module Day02 where

import Data.List.Split
import Data.Vector

type Program = Vector Int

getInput :: IO Program
getInput = fromList <$> (fmap read . (splitOn ",")) <$> readFile "input/day2.txt"

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
                x  = prog ! (prog ! (ptr + 1))
                y  = prog ! (prog ! (ptr + 2))
                z  = prog ! (ptr + 3)

        fix :: Program -> Program
        fix prog = prog // [(1, noun), (2, verb)]

day02a :: Program -> Int
day02a = solve 12 2

day02b :: Program -> Int
day02b prog = noun * 100 + verb
    where
        res1 = solve 0 0 prog
        res2 = solve 1 0 prog
        (noun, verb) = divMod (19690720 - res1) (res2 - res1)

-- Bruteforce Solution for part two
-- day02b :: Program -> Int
-- day02b prog = noun * 100 + verb
--     where (noun, verb) = Prelude.head [(noun,verb) | noun <- [0..99],
--                                                      verb <- [0..99],
--                                                      solve noun verb prog == 19690720]

printSolutions :: IO ()
printSolutions = do
    input <- getInput
    print $ day02a input
    print $ day02b input