{-# LANGUAGE RecordWildCards #-}

module Year2020.Day16 (solution) where

import Data.Either.Combinators (rightToMaybe)
import qualified Data.IntMap as IntMap
import Data.Ix (inRange)
import Data.List (find, isPrefixOf, transpose)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Solution (Solution (Solution))
import Text.Megaparsec (many, notFollowedBy, sepBy, takeWhileP, try)
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char (char, letterChar, newline, spaceChar, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Util (Parser)

type Range = (Int, Int)

type Constraint = (Range, Range)

type Ticket = [Int]

data TicketsInfo = TicketsInfo {constraints :: Map.Map String Constraint, yourTicket :: Ticket, nearby :: [Ticket]} deriving (Show)

parse :: String -> Maybe TicketsInfo
parse = rightToMaybe . P.parse info ""
  where
    info :: Parser TicketsInfo
    info = do
      c <- many constraint
      string "\nyour ticket:\n"
      y <- ticket
      string "\n\nnearby tickets:\n"
      n <- ticket `sepBy` newline
      return $ TicketsInfo (Map.fromList c) y n

    constraint :: Parser (String, Constraint)
    constraint = do
      notFollowedBy newline
      name <- takeWhileP Nothing (/= ':')
      string ": "
      r1 <- range
      string " or "
      r2 <- range
      newline
      return (name, (r1, r2))

    ticket :: Parser Ticket
    ticket = L.decimal `sepBy` char ','

    range :: Parser Range
    range = (,) <$> L.decimal <*> (char '-' *> L.decimal)

invalidFields :: Map.Map String Constraint -> Ticket -> [Int]
invalidFields constraints = filter invalidForAll
  where
    invalidForAll :: Int -> Bool
    invalidForAll f = all (\(r1, r2) -> not (inRange r1 f || inRange r2 f)) (Map.elems constraints)

part1 :: TicketsInfo -> Int
part1 TicketsInfo {..} = sum $ nearby >>= invalidFields constraints

part2 :: TicketsInfo -> Int
part2 TicketsInfo {..} = product final
  where
    validNearby = filter (null . invalidFields constraints) nearby
    possibilities = fmap (Map.keysSet . fit constraints) (transpose validNearby)
    final = fmap (\(i, _) -> yourTicket !! i) (filter (\(_, name) -> "departure" `isPrefixOf` name) (fields possibilities IntMap.empty))

    fields :: [Set.Set String] -> IntMap.IntMap String -> [(Int, String)]
    fields possibilities acc
      | all null possibilities = IntMap.toAscList acc
      | otherwise = fields narrowedDown (IntMap.insert i name acc)
      where
        (forcedConstraint, i) = fromJust $ find (\(p, _) -> Set.size p == 1) (zip possibilities [0 ..])
        name = Set.elemAt 0 forcedConstraint
        narrowedDown = fmap (Set.delete name) possibilities

    fit :: Map.Map String Constraint -> [Int] -> Map.Map String Constraint
    fit constraints column = Map.filter (\(r1, r2) -> all (\f -> inRange r1 f || inRange r2 f) column) constraints

solution :: Solution TicketsInfo Int Int
solution = Solution "Day 16" "input/Year2020/day16.txt" parse part1 part2
