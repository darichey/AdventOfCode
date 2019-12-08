module Day08 (solutions) where

import Data.List.Split
import Data.Char
import Data.List
import Data.Ord

type Pixel = Char
type Layer = [Pixel]
type Image = [Layer]

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

getInput :: IO Image
getInput = chunksOf 150 <$> readFile "input/day8.txt"

day08a :: Image -> Int
day08a layers = ones * twos
    where
        leastZeros = minimumBy (comparing (count '0')) layers
        ones = count '1' leastZeros
        twos = count '2' leastZeros

day08b :: Image -> String
day08b layers = unlines $ chunksOf 25 (fmap (pixelToChar . top layers) [0..149])

top :: Image -> Int -> Pixel
top layers pixel = head $ filter (/= '2') (fmap (!! pixel) layers)

pixelToChar :: Pixel -> Char
pixelToChar '1' = '#'
pixelToChar _   = ' '

solutions :: IO (Int, String)
solutions = do
    input <- getInput
    return (day08a input, day08b input)
