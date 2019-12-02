module Main where

import Day01
import Day02

main :: IO ()
main = do
    printSolutions "Day 1" Day01.solutions
    printSolutions "Day 2" Day02.solutions

printSolutions :: String -> IO (Int, Int) -> IO ()
printSolutions day solsM = do
    (part1, part2) <- solsM
    putStrLn $ day
    putStrLn $ "  " ++ (show part1)
    putStrLn $ "  " ++ (show part2)