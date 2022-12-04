{-# LANGUAGE RecordWildCards #-}
module Year2021.Day05 (solution) where

import Solution (Solution (Solution))
import qualified Data.MultiSet as MultiSet
import Util (Parser, count)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec as P
import Data.Either.Combinators (rightToMaybe)
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char, string, newline)

data LineSegment = LineSegment {  x1 :: Int, y1 :: Int, x2 :: Int, y2 :: Int } deriving Show

parse :: String -> Maybe [LineSegment]
parse = rightToMaybe . P.parse input ""
  where
    input = segment `sepBy` newline

    segment :: Parser LineSegment
    segment = LineSegment <$> L.decimal <*> (char ',' *> L.decimal) <*> (string " -> " *> L.decimal) <*> (char ',' *> L.decimal)

solve :: [LineSegment] -> Int
solve = count (> 1) . (snd <$>) . MultiSet.toOccurList . MultiSet.fromList . (points =<<)
  where
    rangeFromTo :: Int -> Int -> [Int]
    rangeFromTo a b = if a <= b then [a .. b] else [a, a-1..b]

    minMax :: Ord a => a -> a -> (a, a)
    minMax a b = (min a b, max a b)

    points :: LineSegment -> [(Int, Int)]
    points LineSegment{..} = zip xs ys
      where
        ((minX, minY), (maxX, maxY)) = minMax (x1, y1) (x2, y2)
        xs = if minX == maxX then repeat minX else rangeFromTo minX maxX
        ys = if minY == maxY then repeat minY else rangeFromTo minY maxY

part1 :: [LineSegment] -> Int
part1 = solve . filter (\segment -> y1 segment == y2 segment ||  x1 segment == x2 segment)

part2 :: [LineSegment] -> Int
part2 = solve

solution :: Solution [LineSegment] Int Int
solution = Solution "Day 5" "input/Year2021/day5.txt" parse part1 part2
