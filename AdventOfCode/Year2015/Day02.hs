{-# LANGUAGE RecordWildCards #-}
module Year2015.Day02 (solution) where

import Data.List.Split (splitOn)
import Solution (Solution(Solution))
import Data.List (minimumBy)
import Data.Ord (comparing)
data Dims = Dims { l :: Int, w :: Int, h :: Int }

parse :: String -> Maybe [Dims]
parse =  Just . fmap (parseDimList . splitOn "x") . splitOn "\n"
  where
    parseDimList :: [String] -> Dims
    parseDimList [l,w,h] = Dims (read l) (read w) (read h)

part1 :: [Dims] -> Int
part1 = sum . fmap requiredPaper
  where
    requiredPaper Dims{..} = 2*l*w + 2*w*h + 2*h*l + minimum [l*w, w*h, h*l]

part2 :: [Dims] -> Int
part2 = sum . fmap requiredRibbon
  where
    requiredRibbon d@Dims{..} = l*w*h + perimterOfSmallestSide d
    perimterOfSmallestSide Dims{..} = perimeter $ minimumBy (comparing (uncurry (*))) [(l,w),(w,h),(h,l)]
    perimeter (x,y) = x + x + y + y

solution :: Solution [Dims] Int Int
solution = Solution "Day 2" "input/Year2015/day2.txt" parse part1 part2
