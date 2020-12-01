module Year2020.Day01 (solutions) where

getInput :: IO [Int]
getInput = fmap read . lines <$> readFile "input/Year2020/day1.txt"

day01a :: [Int] -> Int
day01a ints = head [x * y | x <- ints, y <- ints, x + y == 2020]

day01b :: [Int] -> Int
day01b ints = head [x * y * z | x <- ints, y <- ints, z <- ints, x + y + z == 2020]

solutions :: IO (Int, Int)
solutions = do
  input <- getInput
  return (day01a input, day01b input)