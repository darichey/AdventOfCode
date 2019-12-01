module Day01 (day01a, day01b, printSolutions) where

getInput :: IO [Int]
getInput = (fmap read . lines) <$> readFile "input/day1.txt"

fuel :: Int -> Int
fuel mass = max ((mass `div` 3) - 2) 0

allFuel :: Int -> Int
allFuel x | x == 0    = 0
          | otherwise = (y + allFuel y) where y = fuel x

day01a :: [Int] -> Int
day01a = sum . fmap fuel

day01b :: [Int] -> Int
day01b = sum . fmap allFuel

printSolutions :: IO ()
printSolutions = do
    input <- getInput
    print $ day01a input
    print $ day01b input