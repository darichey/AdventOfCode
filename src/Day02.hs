module Day02 where

import Data.List
import Data.Maybe

getInput :: IO [Int]
getInput = (fmap read . words . f) <$> readFile "input/day2.txt"
    where f xs = fmap (\x -> if x == ',' then ' ' else x) xs
    
update :: [a] -> Int -> a -> [a]
update (x:xs) idx y =
    if idx == 0 then y:xs
    else x : update xs (idx - 1) y

solve :: Int -> Int -> [Int] -> Int
solve noun verb tape = (day02a' 0 (fix tape)) !! 0
    where
        day02a' :: Int -> [Int] -> [Int]
        day02a' opIdx tape = 
            if op == 99 then tape
            else day02a' (opIdx + 4) (f op opIdx tape)
            where
                op = (tape !! opIdx)

        f :: Int -> Int -> [Int] -> [Int]
        f op idx tape | op == 1 = update tape (tape !! (idx + 3)) ((tape !! (tape !! (idx + 1))) + (tape !! (tape !! (idx + 2)))) 
                      | op == 2 = update tape (tape !! (idx + 3)) ((tape !! (tape !! (idx + 1))) * (tape !! (tape !! (idx + 2))))
                      | otherwise = undefined

        fix :: [Int] -> [Int]
        fix tape = update (update tape 1 noun) 2 verb

day02a :: [Int] -> Int
day02a = solve 12 2

day02b :: [Int] -> (Int, Int)
day02b tape = fromJust $ find (\p -> (solve (fst p) (snd p) tape) == 19690720) pairs
    where pairs = [(x,y) | x <- [0..99], y <- [0..99]]

printSolutions :: IO ()
printSolutions = do
    input <- getInput
    print $ day02a input
    print $ day02b input