module Year2015.Day06 (solution) where

import Data.Either.Combinators (rightToMaybe)
import Data.Functor (($>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Solution (Solution (Solution))
import Text.Megaparsec (sepBy, try, (<|>))
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, newline, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser)

type Point = (Int, Int)

data Instruction
  = On (Set.Set Point)
  | Off (Set.Set Point)
  | Toggle (Set.Set Point)
  deriving (Show)

parse :: String -> Maybe [Instruction]
parse = rightToMaybe . P.parse (instruction `sepBy` newline) ""
  where
    instruction :: Parser Instruction
    instruction = do
      change <-
        try (string "turn on ") $> On
          <|> try (string "turn off ") $> Off
          <|> try (string "toggle ") $> Toggle

      p1 <- point
      string " through "
      p2 <- point

      return $ change (points p1 p2)

    point :: Parser Point
    point = (,) <$> L.decimal <* char ',' <*> L.decimal

    points :: Point -> Point -> Set.Set Point
    points (x1, y1) (x2, y2) = Set.cartesianProduct (Set.fromList [x1 .. x2]) (Set.fromList [y1 .. y2])

part1 :: [Instruction] -> Int
part1 = length . foldl step Set.empty
  where
    step :: Set.Set Point -> Instruction -> Set.Set Point
    step current (On points) = Set.union points current
    step current (Off points) = Set.difference current points
    step current (Toggle points) = symmetricDifference points current

    symmetricDifference :: (Ord a) => Set.Set a -> Set.Set a -> Set.Set a
    symmetricDifference x y = (x Set.\\ y) `Set.union` (y Set.\\ x)

part2 :: [Instruction] -> Int
part2 = sum . foldl step Map.empty
  where
    step :: Map.Map Point Int -> Instruction -> Map.Map Point Int
    step current (On points) = foldl (flip (Map.alter (addOrZero 1))) current points
    step current (Off points) = foldl (flip (Map.alter (addOrZero (-1)))) current points
    step current (Toggle points) = foldl (flip (Map.alter (addOrZero 2))) current points

    addOrZero :: Int -> Maybe Int -> Maybe Int
    addOrZero x (Just y) = Just (max 0 (x + y))
    addOrZero x Nothing = Just (max 0 x)

solution :: Solution [Instruction] Int Int
solution = Solution "Day 6" "input/Year2015/day6.txt" parse part1 part2
