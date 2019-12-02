-- https://adventofcode.com/2019/day/1
module Day01 (solutions) where

-- |Reads the list of modules masses as a [Int]
getInput :: IO [Int]
getInput = (fmap read . lines) <$> readFile "input/day1.txt"

-- |Claculates the simple amount of fuel required for a module
fuel :: Int -> Int
fuel mass = max ((mass `div` 3) - 2) 0

-- |Calculates the total amount of fuel required for a module, taking into
-- account the fact that fuel itself requires more fuel
allFuel :: Int -> Int
allFuel 0 = 0
allFuel x = (y + allFuel y)
    where y = fuel x

day01a :: [Int] -> Int
day01a = sum . fmap fuel

day01b :: [Int] -> Int
day01b = sum . fmap allFuel

solutions :: IO (Int, Int)
solutions = do
    input <- getInput
    return (day01a input, day01b input)