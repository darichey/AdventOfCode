module Day04 where

import Data.List
import Data.List.Split

getInput :: IO (Int, Int)
getInput = firstTwo . fmap read . splitOn "-" <$> readFile "input/day4.txt"
    where firstTwo [x,y] = (x,y)

pairs :: [a] -> [(a, a)]
pairs x = zip x (tail x)

twoAdjacent :: String -> Bool
twoAdjacent = any (uncurry (==)) . pairs

exactlyPair :: String -> Bool
exactlyPair = any (\x -> length x == 2) . group

increasing :: String -> Bool
increasing = all (uncurry (<=)) . pairs

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both f g x = f x && g x

day04a :: [String] -> Int
day04a = length . filter (both increasing twoAdjacent)

day04b :: [String] -> Int
day04b = length . filter (both increasing exactlyPair)

solutions :: IO (Int, Int)
solutions = do
    (a,b) <- getInput
    let nums = fmap show [a..b]
    return (day04a nums, day04b nums)