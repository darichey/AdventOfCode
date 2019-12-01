module Day01 where

fuel :: Int -> Int
fuel x = max ((x `div` 3) - 2) 0

fuel' :: Int -> Int
fuel' x | x == 0    = 0
        | otherwise = (y + fuel' y) where y = fuel x

solve :: (Int -> Int) -> String -> String
solve f = show . sum . fmap (f . read) . lines

day01a :: String -> String
day01a = solve fuel

day01b :: String -> String
day01b = solve fuel'

printSolutions :: IO ()
printSolutions = do
    input <- readFile "input/day1.txt"
    putStrLn $ day01a input
    putStrLn $ day01b input