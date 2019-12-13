{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13

class Solution a where
    showSol :: a -> String

instance {-# OVERLAPPING #-} Solution String where
    showSol = id

instance (Show a) => Solution a where
    showSol = show

main :: IO ()
main = do
    printSolutions "Day 1" Day01.solutions
    printSolutions "Day 2" Day02.solutions
    printSolutions "Day 3" Day03.solutions
    printSolutions "Day 4" Day04.solutions
    printSolutions "Day 5" Day05.solutions
    printSolutions "Day 6" Day06.solutions
    printSolutions "Day 7" Day07.solutions
    printSolutions "Day 8" Day08.solutions
    printSolutions "Day 9" Day09.solutions
    printSolutions "Day 10" Day10.solutions
    printSolutions "Day 11" Day11.solutions
    printSolutions "Day 12" Day12.solutions

printSolutions :: (Solution a, Solution b) => String -> IO (a, b) -> IO ()
printSolutions day solsM = do
    (part1, part2) <- solsM
    putStrLn $ " == " ++ day ++ " == "
    putStrLn $ showSol part1
    putStrLn $ showSol part2