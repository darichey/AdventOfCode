module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06

main :: IO ()
main = do
    printSolutions "Day 1" Day01.solutions
    printSolutions "Day 2" Day02.solutions
    printSolutions "Day 3" Day03.solutions
    printSolutions "Day 4" Day04.solutions
    printSolutions "Day 5" Day05.solutions
    printSolutions "Day 6" Day06.solutions

printSolutions :: String -> IO (Int, Int) -> IO ()
printSolutions day solsM = do
    (part1, part2) <- solsM
    putStrLn $ day
    putStrLn $ "  " ++ (show part1)
    putStrLn $ "  " ++ (show part2)