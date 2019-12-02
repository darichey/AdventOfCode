module Day01 (solutions) where

getInput :: IO [Int]
getInput = (fmap read . lines) <$> readFile "input/day1.txt"

fuel :: Int -> Int
fuel mass = max ((mass `div` 3) - 2) 0

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